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
import Data.List (intersperse)

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
  :: (Typedesc, (a -> Text))
  -- ^ Describes the input type of the resulting 'Pred'
  -> (Typedesc, (b -> Text))
  -- ^ Describes the input type of the input 'Pred'
  -> (a -> b)
  -- ^ Converts the type of the result 'Pred' to the type of the input 'Pred'
  -> Pred b
  -> Pred a
wrap' (descA, shwA) (descB, shwB) conv = C.wrap [fromText lbl] f
  where
    lbl = renderTypedesc descA <+> "is transformed to"
          <+> renderTypedesc descB
    f a = Annotated [fromText dyn] b
      where
        b = conv a
        dyn = "value" <+> shwA a <+> "of type" <+> renderTypedesc descA
          <+> "is transformed to value" <+> shwB b <+> "of type"
          <+> renderTypedesc descB

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
  :: Typedesc
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
    lbl = "value of type" <+> renderTypedesc desc <+> "is" <+> cond
    f a = Annotated [fromText dyn] (pd a)
      where
        dyn = "value" <+> shw a <+> "of type" <+> renderTypedesc desc
          <+> "is" <+> cond

predicate
  :: Text
  -> (a -> Text)
  -> (a -> Bool)
  -> Pred a
predicate st shw pd = C.predicate [fromText st] f
  where
    f a = Annotated [fromText . shw $ a] (pd a)

-- # Lists

anyShower :: Typedesc -> (a -> Text) -> Pred a -> Pred [a]
anyShower desc shwA pd
  = wrap' (List desc, (const "List"))
          (User "Either" [desc, Unit], showEi)
          conv

  $ eitherShower (tyConsCell, showCons)
                 (Unit, X.pack . show)
                 predCons
                 false
  where
    tyConsCell = Tuple2 desc (List desc)
    predCons = predFst ||| predSnd
    predFst = wrap' (tyConsCell, showCons) (desc, shwA) fst pd
    predSnd = wrap' (tyConsCell, showCons)
      (List desc, const "(rest of list)") snd (anyShower desc shwA pd)
    conv ls = case ls of
      [] -> Right ()
      (x:xs) -> Left (x, xs)
    showEi = Prelude.either showCons (const "end of list")
    showCons (x, _) = "Cons cell with head: " <> shwA x


any
  :: Show a
  => Typedesc
  -- ^ Describes the type of the list; for example, if your input list
  -- is @['Int']@, use @Int@ here.
  -> Pred a
  -> Pred [a]
any desc = anyShower desc (X.pack . show)

allShower :: Typedesc -> (a -> Text) -> Pred a -> Pred [a]
allShower desc shwA pd
  = wrap' (List desc, (const "List"))
          (User "Either" [desc, Unit], showEi)
          conv

  $ eitherShower (tyConsCell, showCons)
                 (Unit, X.pack . show)
                 predCons
                 true
  where
    tyConsCell = Tuple2 desc (List desc)
    predCons = predFst &&& predSnd
    predFst = wrap' (tyConsCell, showCons) (desc, shwA) fst pd
    predSnd = wrap' (tyConsCell, showCons)
      (List desc, const "(rest of list)") snd (anyShower desc shwA pd)
    conv ls = case ls of
      [] -> Right ()
      (x:xs) -> Left (x, xs)
    showEi = Prelude.either showCons (const "end of list")
    showCons (x, _) = "Cons cell with head: " <> shwA x


all :: Show a => Typedesc -> Pred a -> Pred [a]
all ty = allShower ty (X.pack . show)

-- # Other Prelude types - maybe, either

maybeShower
  :: Typedesc
  -- ^ Describes type @a@
  -> (a -> Text)
  -> Pred a
  -> Pred (Maybe a)
maybeShower descA shwA
  = wrap' ( User "Maybe" [descA]
          , Prelude.maybe "Nothing" (\x -> "Just" <+> shwA x))
          ( User "Either" [Unit, descA]
          , Prelude.either (const "Left ()") (\x -> "Right" <+> shwA x))
          (Prelude.maybe (Left ()) Right)
  . eitherShower (Unit, (X.pack . show)) (descA, shwA) false

maybe
  :: Show a
  => Typedesc
  -- ^ Describes type @a@
  -> Pred a
  -> Pred (Maybe a)
maybe desc = maybeShower desc (X.pack . show)

eitherShower
  :: (Typedesc, a -> Text)
  -- ^ Describes type @a@
  -> (Typedesc, b -> Text)
  -- ^ Describes type @b@
  -> Pred a
  -> Pred b
  -> Pred (Either a b)
eitherShower (descA, shwA) (descB, shwB) = C.switch [fromText stat] f
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
either dA dB = eitherShower (dA, X.pack . show) (dB, X.pack . show)

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

data Typedesc
  = List Typedesc
  | Unit
  | Tuple2 Typedesc Typedesc
  | Tuple3 Typedesc Typedesc Typedesc
  | Tuple4 Typedesc Typedesc Typedesc Typedesc
  | Tuple5 Typedesc Typedesc Typedesc Typedesc Typedesc
  | User Text [Typedesc]
  deriving (Eq, Ord, Show)

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

renderInnerTypedesc :: Typedesc -> Text
renderInnerTypedesc (User n cs)
  | null cs = n
  | otherwise = "(" <> X.concat (map renderInnerTypedesc cs) <> ")"
renderInnerTypedesc x = renderTypedesc x
