{-# LANGUAGE OverloadedStrings #-}
module Prednote.Prebuilt where

import Rainbow
import qualified Prednote.Core as C
import Prednote.Core (Pred, Annotated(..))
import Data.Text (Text)
import qualified Data.Text as X
import Prelude hiding (any)
import Data.Monoid
import Prednote.Format

-- # Wrapping - handles newtypes

wrap
  :: Text
  -> (a -> Text)
  -> (a -> b)
  -> Pred b
  -> Pred a
wrap st dyn wrapper = C.wrap [fromText st] f
  where
    f a = Annotated [fromText . dyn $ a] (wrapper a)

wrap'
  :: (Text, (a -> Text))
  -- ^ Describes the input type of the resulting 'Pred'
  -> (Text, (b -> Text))
  -- ^ Describes the input type of the input 'Pred'
  -> (a -> b)
  -- ^ Converts the type of the result 'Pred' to the type of the input 'Pred'
  -> Pred b
  -> Pred a
wrap' (descA, shwA) (descB, shwB) conv = C.wrap [fromText lbl] f
  where
    lbl = descA <+> "is transformed to" <+> descB
    f a = Annotated [fromText dyn] b
      where
        b = conv a
        dyn = descA <+> shwA a <+> "is transformed to" <+> descB
          <+> shwB b

-- # Constants

true :: Pred a
true = predicate l (const l) (const True)
  where l = "always True"

false :: Pred a
false = predicate l (const l) (const False)
  where l = "always False"

same :: Pred Bool
same = predicate l (const l) id
  where l = "same as subject"

-- # Predicates

predicate'
  :: Text
  -- ^ Describes the type being matched by the predicate, e.g. 'Int'.
  -- Used for the static label.
  -> Text
  -- ^ Describes the condition the type being matched must meet,
  -- e.g. @greater than 5@.  Used in both the static and dynamic
  -- labels.
  -> (a -> Text)
  -- ^ Describes the value being matched, such as @'X.pack' . 'show'@.
  -- Used for the dynamic label.
  -> (a -> Bool)
  -- ^ Predicate
  -> Pred a
predicate' desc cond shw pd = C.predicate [fromText lbl] f
  where
    lbl = desc <> " is " <> cond
    f a = Annotated [fromText dyn] (pd a)
      where
        dyn = desc <> " " <> shw a <> " is " <> cond

predicate
  :: Text
  -> (a -> Text)
  -> (a -> Bool)
  -> Pred a
predicate st shw pd = C.predicate [fromText st] f
  where
    f a = Annotated [fromText . shw $ a] (pd a)

-- # Lists

anyShower :: Text -> (a -> Text) -> Pred a -> Pred [a]
anyShower desc shwA pd
  = wrap' (listDesc, (const "List"))
          ("Either (" <> desc <> ", " <> listDesc <> ") ()", showEi)
          conv

  $ eitherShower ("(" <> desc <> ", (List " <> desc <> "))")
                 "Nil"
                 showCons
                 (const "[]")
                 predCons
                 false
  where
    predCons = predFst ||| predSnd
    predFst = wrap' ("cons cell", showCons) (desc, shwA) fst pd
    predSnd = wrap' ("cons cell", showCons) (listDesc, const listDesc) snd
      (anyShower desc shwA pd)
    listDesc = "[" <> desc <> "]"
    conv ls = case ls of
      [] -> Right ()
      (x:xs) -> Left (x, xs)
    showEi ei = case ei of
      Left cons -> showCons cons
      Right () -> "End of list"
    showCons (x, _) = "Cons cell with head: " <> shwA x


any
  :: Show a
  => Text
  -- ^ Describes the type of the list; for example, if your input list
  -- is @['Int']@, use @Int@ here.
  -> Pred a
  -> Pred [a]
any desc = anyShower desc (X.pack . show)

allShower :: (a -> Text) -> Pred a -> Pred [a]
allShower = undefined


all :: Show a => Pred a -> Pred [a]
all = allShower (X.pack . show)

-- # Other Prelude types - maybe, either

maybeShower
  :: Text
  -- ^ Describes type @a@
  -> (a -> Text)
  -> Pred a
  -> Pred (Maybe a)
maybeShower descA shwA
  = wrap' ( "Maybe" <+> descA
          , Prelude.maybe "Nothing" (\x -> "Just" <+> shwA x))
          ( "Either ()" <+> descA
          , Prelude.either (const "Left ()") (\x -> "Right" <+> shwA x))
          (Prelude.maybe (Left ()) Right)
  . eitherShower "()" descA (X.pack . show) shwA false

maybe
  :: Show a
  => Text
  -- ^ Describes type @a@
  -> Pred a
  -> Pred (Maybe a)
maybe desc = maybeShower desc (X.pack . show)

eitherShower
  :: Text
  -- ^ Describes type @a@
  -> Text
  -- ^ Describes type @b@
  -> (a -> Text)
  -> (b -> Text)
  -> Pred a
  -> Pred b
  -> Pred (Either a b)
eitherShower descA descB shwA shwB = C.switch [fromText stat] f
  where
    stat = "Either" <+> descA <+> descB
    f a = Annotated [fromText dyn] val
      where
        dyn = "value has type" <+> side <+> ("(" <> typeDesc <> ")")
          <+> "with value" <+> shown
        (side, typeDesc, shown, val) = case a of
          Left l -> ("Left", descA, shwA l, Left l)
          Right r -> ("Right", descB, shwB r, Right r)

either
  :: (Show a, Show b)
  => Text
  -- ^ Describes type A
  -> Text
  -- ^ Describes type B
  -> Pred a
  -> Pred b
  -> Pred (Either a b)
either shwA shwB = eitherShower shwA shwB (X.pack . show) (X.pack . show)

-- # Combining or modifying Pred

(&&&) :: Pred a -> Pred a -> Pred a
l &&& r = C.and t (const t) l r
  where t = ["and - both children must be True"]

infixr 3 &&&

(|||) :: Pred a -> Pred a -> Pred a
l ||| r = C.or t (const t) l r
  where t = ["or - either child must be True"]

infixr 2 |||

not :: Pred a -> Pred a
not = C.not l (const l)
  where l = ["not - child must be False"]

