{-# LANGUAGE OverloadedStrings #-}
module Prednote.Prebuilt
  ( -- * Describing types
    Typedesc(..)
  , renderTypedesc
  , renderInnerTypedesc
  , Typeshow(..)
  , showType
  , showValue
  , typeshow

  -- * Predicates
  , Pdct(..)
  , predicate
  , true
  , false
  , same

  -- * Predicate combinators
  --
  -- ** Primitive combinators
  --
  -- | You might consider these combinators to be \"primitive\" in the
  -- sense that you can build a 'Pdct' for any user-defined type by
  -- using these combinators alone.  Use '&&&', '|||', and 'wrap' to
  -- analyze product types.  Use 'either' and 'wrap' to analyze sum
  -- types.  For a simple example, see the source code for 'maybe',
  -- which is a simple sum type.  For more complicated examples, see
  -- the source code for 'any' and 'all', as a list is a sum type
  -- where one of the summands is a (recursive!) product type.
  , wrap
  , (&&&)
  , (|||)
  , not
  , either

  -- ** Convenience combinators
  --
  -- | These were written using entirely the \"primitive\" combinators
  -- given above.
  , anyOfPair
  , bothOfPair
  , any
  , all
  , maybe

  -- * Running predicates
  , test
  ) where

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

-- # Describing types

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

-- | A type description, with how to show values of that type.
data Typeshow a = Typeshow Typedesc (a -> Text)

-- | Show a type's description.
showType :: Typeshow a -> Text
showType (Typeshow t _) = renderTypedesc t

-- | Show a value of a given type.
showValue :: Typeshow a -> a -> Text
showValue ts@(Typeshow _ f) a = showType ts <+> "of value" <+> f a

-- | Creates a 'Typedesc' for a member of the 'Show' class, when
-- applied to how to describe that type.
typeshow :: Show a => Typedesc -> Typeshow a
typeshow d = Typeshow d (X.pack . show)

instance Show (Typeshow a) where
  show (Typeshow d _) = "Typeshow (" <> show d <> ")"

-- # Predicates

-- | A predicate; contains information about the underlying predicate
-- function, how to describe the type, and how to show values of that
-- type.
data Pdct a = Pdct (C.Pred a) (Typeshow a)
  deriving Show

-- | Creates new 'Pdct'.  The most important 'Pdct' creating function.
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
    lbl = "value of type" <+> showType ts <+> "is" <+> cond
    f a = Annotated [fromText dyn] (pd a)
      where
        dyn = showValue ts a <+> cond

-- | Always returns 'True'.
true :: Pdct a
true = predicate (Typeshow (User "a" []) (const "unknown"))
  "is ignored - always returns True"
  (const True)

-- | Always returns 'False'.
false :: Pdct a
false = predicate (Typeshow (User "a" []) (const "unknown"))
  "is ignored - always returns False"
  (const False)

-- | Always returns the same value that was passed in.
same :: Pdct Bool
same = predicate (Typeshow (User "Bool" []) (X.pack . show))
  "is returned as is" id

-- # Predicate combinators

-- | Creates a new 'Pdct' that unwraps an input type and applies a
-- 'Pdct' to the result of the unwrapping.
wrap
  :: Typeshow a
  -- ^ Describes the input type of the resulting 'Pred'
  -> (a -> b)
  -- ^ Converts the type of the result 'Pred' to the type of the input
  -- 'Pred'; the unwrapper.
  -> Pdct b
  -- ^ Is applied to unwrapped values
  -> Pdct a
wrap tsA conv pd = Pdct (C.wrap [fromText lbl] f pb) tsA
  where
    Pdct pb tsB = pd
    lbl = showType tsA <+> "is transformed to" <+> showType tsB
    f a = Annotated [fromText dyn] b
      where
        b = conv a
        dyn = showValue tsA a
          <+> "is transformed to" <+> showValue tsB b


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

newtype Combiner a = Combiner (Pdct a -> Pdct a -> Pdct a)

-- | Takes an incoming value and splits it into two values.  Each
-- respective 'Pdct' is applied to the result of the split.  Either
-- 'Pdct' must evaluate to 'True'.
anyOfPair
  :: Typeshow o
  -- ^ Description and how to show values of the resulting 'Pdct'
  -> (o -> (a, b))
  -- ^ Splits incoming values into two values
  -> Pdct a
  -- ^ Is applied to one of the results of the split
  -> Pdct b
  -- ^ Is applied to the other result of the split
  -> Pdct o
anyOfPair = predOnPair (Combiner (|||))

-- | Takes an incoming value and splits it into two values.  Each
-- respective 'Pdct' is applied to the result of the split.  Both
-- 'Pdct' must evaluate to 'True'.
bothOfPair
  :: Typeshow o
  -- ^ Description and how to show values of the resulting 'Pdct'
  -> (o -> (a, b))
  -- ^ Splits incoming values into two values
  -> Pdct a
  -- ^ Is applied to one of the results of the split
  -> Pdct b
  -- ^ Is applied to the other result of the split
  -> Pdct o
bothOfPair = predOnPair (Combiner (&&&))

-- | The first 'Pdct' is applied if the value is a 'Left'; the second
-- 'Pdct' is applied if the value is a 'Right'.
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

-- | Like 'Prelude.any'.  'True' if any value in the list is 'True';
-- 'False' if the list is empty.  Short circuits if any value in the
-- list is 'True'.
any :: Pdct a -> Pdct [a]
any = listPred false (Combiner (|||))

-- | Like 'Prelude.all'.  'True' if no value in the list is 'False';
-- 'True' if the list is empty.  Short circuits if any value in the
-- list is 'False'.
all :: Pdct a -> Pdct [a]
all = listPred true (Combiner (&&&))

-- | Returns the result of the first 'Pdct' if the value is 'Nothing';
-- returns the result of applying the second predicate if the value is
-- 'Just'.
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


-- # Running predicates

-- | Applies a 'Pdct' to a value.
test :: Pdct a -> a -> Bool
test (Pdct p _) = C.test p
