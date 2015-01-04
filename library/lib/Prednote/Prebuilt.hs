{-# LANGUAGE OverloadedStrings #-}
module Prednote.Prebuilt where

import Rainbow
import qualified Prednote.Core as C
import Prednote.Core (Pred, Annotated(..))
import Data.Text (Text)
import qualified Data.Text as X
import Prelude hiding (any)

predicate
  :: Text
  -> (a -> Text)
  -> (a -> Bool)
  -> Pred a
predicate st shw pd = C.predicate [fromText st] f
  where
    f a = Annotated [fromText . shw $ a] (pd a)

true :: Pred a
true = predicate l (const l) (const True)
  where l = "always True"

false :: Pred a
false = predicate l (const l) (const False)
  where l = "always False"

same :: Pred Bool
same = predicate l (const l) id
  where l = "same as subject"

anyShower :: (a -> Text) -> Pred a -> Pred [a]
anyShower shw pd = C.switch [fromText stat] caser false pc
  where
    stat = "at least one item must be True"
    caser a = case a of
      [] -> Annotated ["End of list"] (Left ())
      x:xs -> Annotated ["cons cell"] (Right (x, xs))
    pc = C.splitOr ["analyzing cons cell"] fOr pd (anyShower shw pd)
      where
        fOr (x, xs) = Annotated ["cons cell: ", fromText . shw $ x]
                                 (x, xs)

any :: Show a => Pred a -> Pred [a]
any = anyShower (X.pack . show)

allShower :: (a -> Text) -> Pred a -> Pred [a]
allShower shw pd = C.switch [fromText stat] caser true pc
  where
    stat = "no item may be False"
    caser a = case a of
      [] -> Annotated ["End of list"] (Left ())
      x:xs -> Annotated ["cons cell"] (Right (x, xs))
    pc = C.splitAnd ["analyzing cons cell"] fAnd pd (anyShower shw pd)
      where
        fAnd (x, xs) = Annotated ["cons cell: ", fromText . shw $ x]
                                 (x, xs)

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

all :: Show a => Pred a -> Pred [a]
all = allShower (X.pack . show)

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

wrap
  :: Text
  -> (a -> Text)
  -> (a -> b)
  -> Pred b
  -> Pred a
wrap st dyn wrapper = C.wrap [fromText st] f
  where
    f a = Annotated [fromText . dyn $ a] (wrapper a)

