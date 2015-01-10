{-# LANGUAGE OverloadedStrings #-}
module Prednote.Prebuilt.Internal where

import Rainbow
import qualified Prednote.Core as C
import Prednote.Core (Annotated(..))
import Data.Text (Text)
import qualified Data.Text as X
import Prelude
  ( Bool(..), const, Show(..), id, ($), (.), fst, snd,
    Either(..), Maybe(..), Ord(..), Eq(..), map, null,
    otherwise )
import Data.Monoid
import Prednote.Format
import Data.List (intersperse)

-- | A type to describe types.
data Typedesc
  = List Typedesc
  -- ^ Lists; for example, for @['Int']@, use
  --
  -- > List (User "Int") []
  | Unit
  -- ^ The unit type, @()@.
  | Tuple2 Typedesc Typedesc
  -- ^ A tuple; for example, for @('Int', 'Char')@, use
  --
  -- > Tuple2 (User "Int" []) (User "Char" [])
  | Tuple3 Typedesc Typedesc Typedesc
  | Tuple4 Typedesc Typedesc Typedesc Typedesc
  | Tuple5 Typedesc Typedesc Typedesc Typedesc Typedesc
  | User Text [Typedesc]
  -- ^ Any user defined type; also covers types in the standard
  -- library that (unlike @[]@ and @(,)@, for example) do not have
  -- special syntax.  Arbitrary nesting is allowed. Some examples:
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- > -- String
  -- > User "String" []
  -- > -- Maybe Int
  -- > User "Maybe" [User "Int" []]
  -- > -- Maybe a
  -- > User "Maybe" [User "a" []]
  -- > -- Maybe [Either Int Char]
  -- > User "Maybe" [List (User "Either" [User "Int", User "Char"])]
  deriving (Eq, Ord, Show)

-- | Renders a 'Typedesc' so it looks as it would appear in a type
-- signature.
renderTypedesc :: Typedesc -> Text
renderTypedesc (List t) = "[" <> renderTypedesc t <> "]"
renderTypedesc Unit = "()"
renderTypedesc (Tuple2 x y) = "(" <> renderTypedesc x
  <> ", " <> renderTypedesc y <> ")"
renderTypedesc (Tuple3 x y z) = "(" <> renderTypedesc x
  <> ", " <> renderTypedesc y <> ", " <> renderTypedesc z <> ")"
renderTypedesc (Tuple4 w x y z) = "(" <> renderTypedesc w
  <> ", " <> renderTypedesc x <> ", " <> renderTypedesc y
  <> ", " <> renderTypedesc z <> ")"
renderTypedesc (Tuple5 v w x y z) = "(" <> renderTypedesc v <> ", "
  <> renderTypedesc w <> ", " <> renderTypedesc x <> ", "
  <> renderTypedesc y <> ", " <> renderTypedesc z <> ")"
renderTypedesc (User n cs)
  = n <+> X.concat (intersperse " " . map renderInnerTypedesc $ cs)

-- | Renders a 'Typedesc', as it would appear if it is a parameter
-- type of a user-defined type.  This means that a user-defined
-- 'Typedesc' is rendered with surrounding parentheses if necessary;
-- all other types are rendered just as they would be by
-- 'renderTypedesc'.
renderInnerTypedesc :: Typedesc -> Text
renderInnerTypedesc (User n cs)
  | null cs = n
  | otherwise = "(" <> X.concat (map renderInnerTypedesc cs) <> ")"
renderInnerTypedesc x = renderTypedesc x

data Typeshow a = Typeshow Typedesc (a -> Text)

describe :: Typeshow a -> Text
describe (Typeshow t _) = renderTypedesc t

showValue :: Typeshow a -> a -> Text
showValue ts@(Typeshow _ f) a = describe ts <+> "of value" <+> f a

typeshow :: Show a => Typedesc -> Typeshow a
typeshow d = Typeshow d (X.pack . show)

instance Show (Typeshow a) where
  show (Typeshow d _) = "Typeshow (" <> show d <> ")"

data Pdct a = Pdct (C.Pred a) (Typeshow a)
  deriving Show


wrap
  :: Typeshow a
  -- ^ Describes the input type of the resulting 'Pred'
  -> (a -> b)
  -- ^ Converts the type of the result 'Pred' to the type of the input 'Pred'
  -> Pdct b
  -> Pdct a
wrap tsA conv pd = Pdct (C.wrap [fromText lbl] f pb) tsA
  where
    Pdct pb tsB = pd
    lbl = describe tsA <+> "is transformed to" <+> describe tsB
    f a = Annotated [fromText dyn] b
      where
        b = conv a
        dyn = showValue tsA a
          <+> "is transformed to" <+> showValue tsB b


predOnPair
  :: Combiner (a, b)
  -> Typeshow o
  -> (o -> (a, b))
  -> Pdct a
  -> Pdct b
  -> Pdct o

predOnPair (Combiner comb) tso split pa pb
  = wrap tso split (pa' `comb` pb')
  where
    Pdct _ (Typeshow descA shwA) = pa
    Pdct _ (Typeshow descB shwB) = pb
    tsComb = Typeshow descTup showTup
    showTup (a, b) = "(" <> shwA a <> ", " <> shwB b <> ")"
    descTup = Tuple2 descA descB
    pa' = wrap tsComb fst pa
    pb' = wrap tsComb snd pb

-- | And - both children must be 'True'; short-circuits if the first
-- child is 'False'
(&&&) :: Pdct a -> Pdct a -> Pdct a
pdL &&& pdR = Pdct (C.and t (const t) pL pR) dL
  where
    Pdct pL dL = pdL
    Pdct pR _ =  pdR
    t = ["and - both children must be True"]

infixr 3 &&&

-- | Or - either child must be 'True'; short-circuits if the first
-- child is 'True'
(|||) :: Pdct a -> Pdct a -> Pdct a
pdL ||| pdR = Pdct (C.or t (const t) pL pR) dL
  where
    t = ["or - either child must be True"]
    Pdct pL dL = pdL
    Pdct pR _ = pdR

infixr 2 |||

-- | Negation - child must be 'False'
not :: Pdct a -> Pdct a
not pd = Pdct (C.not l (const l) p) d
  where
    l = ["not - child must be False"]
    Pdct p d = pd


anyOfPair
  :: Typeshow o
  -> (o -> (a, b))
  -> Pdct a
  -> Pdct b
  -> Pdct o
anyOfPair = predOnPair (Combiner (|||))

bothOfPair
  :: Typeshow o
  -> (o -> (a, b))
  -> Pdct a
  -> Pdct b
  -> Pdct o
bothOfPair = predOnPair (Combiner (&&&))

predicate
  :: Typeshow a
  -- ^ Description of the type of the resulting 'Pdct', and how to
  -- show it.
  -> Text
  -- ^ Describes the condition; for example, for a @'Pdct' 'Int'@,
  -- this might be @is greater than 5@; for a @'Pdct' 'String'@, this
  -- might be @begins with \"Hello\"@.
  -> (a -> Bool)
  -- ^ The predicate.
  -> Pdct a
predicate ts cond pd = Pdct (C.predicate [fromText lbl] f) ts
  where
    lbl = "value of type" <+> describe ts <+> "is" <+> cond
    f a = Annotated [fromText dyn] (pd a)
      where
        dyn = showValue ts a <+> cond

true :: Pdct a
true = predicate (Typeshow (User "a" []) (const "unknown"))
  "is ignored - always returns True"
  (const True)

false :: Pdct a
false = predicate (Typeshow (User "a" []) (const "unknown"))
  "is ignored - always returns False"
  (const False)

same :: Pdct Bool
same = predicate (Typeshow (User "Bool" []) (X.pack . show))
  "is returned as is" id

either
  :: Pdct a
  -> Pdct b
  -> Pdct (Either a b)
either pdctA pdctB = Pdct (C.switch [fromText stat] f pA pB) tw
  where
    Pdct pA tsA = pdctA
    Pdct pB tsB = pdctB
    Typeshow descA shwA = tsA
    Typeshow descB shwB = tsB
    stat = renderTypedesc (User "Either" [descA, descB])
    f a = Annotated [fromText (shwEi a)] a
    tw = Typeshow (User "Either" [descA, descB]) shwEi
    shwEi ei = case ei of
      Left x -> "Left (" <> shwA x <> ")"
      Right x -> "Right (" <> shwB x <> ")"

newtype Combiner a = Combiner (Pdct a -> Pdct a -> Pdct a)

consCellPred
  :: Pdct a
  -> Pdct [a]
  -> Combiner (a, [a])
  -> Pdct (a, [a])
consCellPred pOne pLs comb
  = predOnPair comb tw id pOne pLs
  where
    Pdct _ (Typeshow descA shwA) = pOne
    Pdct _ (Typeshow descLs _) = pLs
    tw = Typeshow (Tuple2 descA descLs) f
    f (x, _) = "cons cell with head: " <> shwA x


listPred
  :: Pdct ()
  -- ^ What to do on an empty list
  -> Combiner (a, [a])
  -- ^ How to combine a 'Pdct' on an item and a 'Pdct' on the rest of the list
  -> Pdct a
  -> Pdct [a]
listPred pEmpty comb pa
  = wrap tsLs toEi
  $ either pCons pEmpty
  where
    Pdct _ (Typeshow descA _) = pa
    tsLs = Typeshow (List descA) (const "List")
    toEi ls = case ls of
      x:xs -> Left (x, xs)
      [] -> Right ()
    pCons = consCellPred pa (listPred pEmpty comb pa) comb

any :: Pdct a -> Pdct [a]
any = listPred false (Combiner (|||))

all :: Pdct a -> Pdct [a]
all = listPred true (Combiner (&&&))

maybe :: Pdct () -> Pdct a -> Pdct (Maybe a)
maybe pNothing pa
  = wrap tsMaybe toEi
  $ either pa pNothing
  where
    Pdct _ (Typeshow descA shwA) = pa
    tsMaybe = Typeshow (User "Maybe" [descA]) shwMaybe
    shwMaybe a = case a of
      Nothing -> "Nothing"
      Just x -> "Just (" <> shwA x <> ")"
    toEi a = case a of
      Nothing -> Right ()
      Just x -> Left x

test :: Pdct a -> a -> Bool
test (Pdct p _) = C.test p
