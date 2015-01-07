{-# LANGUAGE OverloadedStrings #-}
module Prednote.Prebuilt.Internal where

import Rainbow
import qualified Prednote.Core as C
import Prednote.Core (Pred, Annotated(..))
import Data.Text (Text)
import qualified Data.Text as X
import qualified Prelude
import Prelude
  ( Bool(..), const, Show(..), id, ($), (.), fst, snd,
    Either(..), Maybe(..), Ord(..), Eq(..), map, null,
    otherwise, undefined )
import Data.Monoid
import Prednote.Format
import Data.List (intersperse)

data Typeshow a = Typeshow Typedesc (a -> Text)

describe :: Typeshow a -> Text
describe (Typeshow t _) = renderTypedesc t

showValue :: Typeshow a -> a -> Text
showValue ts@(Typeshow _ f) a = describe ts <+> "of value" <+> f a

instance Show (Typeshow a) where
  show (Typeshow d _) = "Typeshow (" <> show d <> ")"

data Pdct a = Pdct (C.Pred a) (Typeshow a)
  deriving Show


-- # Wrapping - handles newtypes

wrap
  :: Typeshow a
  -- ^ Describes the input type of the resulting 'Pred'
  -> Typeshow b
  -- ^ Describes the input type of the input 'Pred'
  -> (a -> b)
  -- ^ Converts the type of the result 'Pred' to the type of the input 'Pred'
  -> Pred b
  -> Pred a
wrap tsA tsB conv = C.wrap [fromText lbl] f
  where
    lbl = describe tsA <+> "is transformed to" <+> describe tsB
    f a = Annotated [fromText dyn] b
      where
        b = conv a
        dyn = showValue tsA a
          <+> "is transformed to" <+> showValue tsB b

anyOfPair
  :: Typeshow a
  -> Typeshow b
  -> Typeshow o
  -> (o -> (a, b))
  -> Pred a
  -> Pred b
  -> Pred o
anyOfPair = predOnPair (|||)

bothOfPair
  :: Typeshow a
  -> Typeshow b
  -> Typeshow o
  -> (o -> (a, b))
  -> Pred a
  -> Pred b
  -> Pred o
bothOfPair = predOnPair (&&&)


predOnPair
  :: (Pred (a, b) -> Pred (a, b) -> Pred (a, b))  
  -> Typeshow a
  -> Typeshow b
  -> Typeshow o
  -> (o -> (a, b))
  -> Pred a
  -> Pred b
  -> Pred o

predOnPair comb tsA@(Typeshow descA shwA) tsB@(Typeshow descB shwB)
  tsO split pa pb = wrap tsO tsComb split (pa' `comb` pb')
  where
    tsComb = Typeshow descTup showTup
    showTup (a, b) = "(" <> shwA a <> ", " <> shwB b <> ")"
    descTup = Tuple2 descA descB
    pa' = wrap tsComb tsA fst pa
    pb' = wrap tsComb tsB snd pb

-- # Constants

true :: Pred a
true = predicate (Typeshow (User "a" []) (const "unknown"))
  "ignored - always returns True"
  (const True)

false :: Pred a
false = predicate (Typeshow (User "a" []) (const "unknown"))
  "ignored - always returns False"
  (const False)

same :: Pred Bool
same = predicate (Typeshow (User "Bool" []) (X.pack . show))
  "returned as is" id

-- # Predicates

predicate
  :: Typeshow a
  -> Text
  -- ^ Describes the condition the type being matched must meet,
  -- e.g. @greater than 5@.  Used in both the static and dynamic
  -- labels.
  -> (a -> Bool)
  -- ^ Predicate
  -> Pred a
predicate ts cond pd = C.predicate [fromText lbl] f
  where
    lbl = "value of type" <+> describe ts <+> "is" <+> cond
    f a = Annotated [fromText dyn] (pd a)
      where
        dyn = showValue ts a <+> "is" <+> cond

predicate'
  :: Typeshow a
  -> Text
  -> (a -> Bool)
  -> Pdct a
predicate' ts cond pd = Pdct (C.predicate [fromText lbl] f) ts
  where
    lbl = "value of type" <+> describe ts <+> "is" <+> cond
    f a = Annotated [fromText dyn] (pd a)
      where
        dyn = showValue ts a <+> "is" <+> cond

consCellPred
  :: Pred [a]
  -> Typeshow a
  -> Pred a
  -> (Pred (a, [a]) -> Pred (a, [a]) -> Pred (a, [a]))
  -> Pred (a, [a])
consCellPred pLs ts@(Typeshow tA shwA) pd comb
  = xOfPair ts tsLs tsPair id pd pLs
  where
    tsLs = Typeshow (List tA) (const "(rest of list")
    tsPair = Typeshow (Tuple2 tA (List tA))
      (\(x, _) -> "cons cell with head: " <> shwA x)
    xOfPair = predOnPair comb


-- # Lists

anyShower :: Typeshow a -> Pred a -> Pred [a]
anyShower tw@(Typeshow desc shwA) pd
  = eiPredToListPred tw
  $ eitherShower (Typeshow tyConsCell showCons)
                 (Typeshow Unit (X.pack . show))
                 predCons
                 false
  where
    tyConsCell = Tuple2 desc (List desc)
    predCons = predFst ||| predSnd
    predFst = wrap (Typeshow tyConsCell showCons) tw fst pd
    predSnd = wrap (Typeshow tyConsCell showCons)
      (Typeshow (List desc) (const "(rest of list)"))
      snd (anyShower tw pd)
    showCons (x, _) = "Cons cell with head: " <> shwA x


eiPredToListPred
  :: Typeshow a
  -> Pred (Either (a, [a]) ())
  -> Pred [a]
eiPredToListPred (Typeshow desc shwA) = wrap listDesc eiDesc conv
  where
    listDesc = Typeshow (List desc) (const "List")
    eiDesc = Typeshow (User "Either" [Tuple2 desc (List desc), Unit]) showEi
    showEi ei = case ei of
      Left (x, _) -> "cons cell with head: " <> shwA x
      Right () -> "(end of list)"
    conv ls = case ls of
      [] -> Right ()
      (x:xs) -> Left (x, xs)

any
  :: Show a
  => Typedesc
  -- ^ Describes the type of the list; for example, if your input list
  -- is @['Int']@, use @Int@ here.
  -> Pred a
  -> Pred [a]
any desc = anyShower (Typeshow desc (X.pack . show))

allShower :: Typeshow a -> Pred a -> Pred [a]
allShower tw@(Typeshow desc shwA) pd
  = eiPredToListPred tw
  $ eitherShower twCons
                 (Typeshow Unit (X.pack . show))
                 predCons
                 true
  where
    twCons = Typeshow (Tuple2 desc (List desc))
      (\(x, _) -> "Cons cell with head: " <> shwA x)
    predCons = predFst &&& predSnd
    predFst = wrap twCons tw fst pd
    predSnd = wrap twCons
      (Typeshow (List desc) (const "(rest of list)"))
      snd (allShower tw pd)


all :: Show a => Typedesc -> Pred a -> Pred [a]
all ty = allShower (Typeshow ty (X.pack . show))

-- # Other Prelude types - maybe, either

maybeShower
  :: Typeshow a
  -> Pred a
  -> Pred (Maybe a)
maybeShower tw@(Typeshow descA shwA)
  = wrap ( Typeshow (User "Maybe" [descA])
          (Prelude.maybe "Nothing" (\x -> "Just" <+> shwA x)))
          ( Typeshow (User "Either" [Unit, descA])
            (Prelude.either (const "Left ()") (\x -> "Right" <+> shwA x)))
          (Prelude.maybe (Left ()) Right)
  . eitherShower (Typeshow Unit (X.pack . show)) tw false

maybe
  :: Show a
  => Typedesc
  -- ^ Describes type @a@
  -> Pred a
  -> Pred (Maybe a)
maybe desc = maybeShower (Typeshow desc (X.pack . show))

eitherShower
  :: Typeshow a
  -- ^ Describes type @a@
  -> Typeshow b
  -- ^ Describes type @b@
  -> Pred a
  -> Pred b
  -> Pred (Either a b)
eitherShower (Typeshow descA shwA) (Typeshow descB shwB)
  = C.switch [fromText stat] f
  where
    stat = renderTypedesc (User "Either" [descA, descB])
    f a = Annotated [fromText dyn] val
      where
        dyn = "value of" <+> shown <+> "has type"
          <+> renderTypedesc (User "Either" [descA, descB])
        (shown, val) = case a of
          Left l -> (shwA l, Left l)
          Right r -> (shwB r, Right r)

either
  :: (Show a, Show b)
  => Typedesc
  -- ^ Describes type @a@
  -> Typedesc
  -- ^ Describes type @b@
  -> Pred a
  -> Pred b
  -> Pred (Either a b)
either dA dB = eitherShower (Typeshow dA (X.pack . show))
                            (Typeshow dB (X.pack . show))

-- # Combining or modifying Pred

-- | And - both children must be 'True'; short-circuits if the first
-- child is 'False'
(&&&) :: Pred a -> Pred a -> Pred a
l &&& r = C.and t (const t) l r
  where t = ["and - both children must be True"]

infixr 3 &&&

-- | Or - either child must be 'True'; short-circuits if the first
-- child is 'True'
(|||) :: Pred a -> Pred a -> Pred a
l ||| r = C.or t (const t) l r
  where t = ["or - either child must be True"]

infixr 2 |||

-- | Negation - child must be 'False'
not :: Pred a -> Pred a
not = C.not l (const l)
  where l = ["not - child must be False"]

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
