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
  :: Text
  -- ^ Describes the input type of the resulting 'Pred'
  -> Text
  -- ^ Describes the input type of the input 'Pred'
  -> (a -> Text)
  -- ^ Shows the input type of the resulting 'Pred'
  -> (b -> Text)
  -- ^ Shows the input type of the input 'Pred'
  -> (a -> b)
  -- ^ Converts the type of the result 'Pred' to the type of the input 'Pred'
  -> Pred b
  -> Pred a
wrap' descA descB shwA shwB conv = C.wrap [fromText lbl] f
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

anyShower :: (a -> Text) -> Pred a -> Pred [a]
anyShower shw pd = C.switch [fromText stat] caser false pc
  where
    stat = "either head or tail of (:) must be True ([] always False)"
    caser a = case a of
      [] -> Annotated ["[]"] (Left ())
      x:xs -> Annotated ["(:)"] (Right (x, xs))
    pc = C.splitOr ["analyzing cons cell"] fOr pd (anyShower shw pd)
      where
        fOr (x, xs) = Annotated ["cons cell: ", fromText . shw $ x]
                                 (x, xs)

any :: Show a => Pred a -> Pred [a]
any = anyShower (X.pack . show)

allShower :: (a -> Text) -> Pred a -> Pred [a]
allShower shw pd = C.switch [fromText stat] caser true pc
  where
    stat = "both head and tail of (:) must be True ([] always True)"
    caser a = case a of
      [] -> Annotated ["End of list"] (Left ())
      x:xs -> Annotated ["cons cell"] (Right (x, xs))
    pc = C.splitAnd ["analyzing cons cell"] fAnd pd (anyShower shw pd)
      where
        fAnd (x, xs) = Annotated ["cons cell: ", fromText . shw $ x]
                                 (x, xs)

all :: Show a => Pred a -> Pred [a]
all = allShower (X.pack . show)

-- # Other Prelude types - maybe, either

maybeShower :: (a -> Text) -> Pred a -> Pred (Maybe a)
maybeShower shw = C.switch [stat] caser false
  where
    stat = "False on Nothing; test with child predicate on Just"
    caser a = case a of
      Nothing -> Annotated ["Nothing"] (Left ())
      Just x -> Annotated ["Just: ", fromText . shw $ x] (Right x)

maybe :: Show a => Pred a -> Pred (Maybe a)
maybe = maybeShower (X.pack . show)

eitherShower
  :: (a -> Text)
  -> (b -> Text)
  -> Pred a
  -> Pred b
  -> Pred (Either a b)
eitherShower shwA shwB = C.switch ["either"] f
  where
    f ei = case ei of
      Left l -> Annotated ["Left: ", fromText . shwA $ l] (Left l)
      Right r -> Annotated ["Right: ", fromText . shwB $ r] (Right r)

either :: (Show a, Show b) => Pred a -> Pred b -> Pred (Either a b)
either = eitherShower (X.pack . show) (X.pack . show)

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

