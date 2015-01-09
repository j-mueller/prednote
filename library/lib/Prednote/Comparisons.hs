{-# LANGUAGE OverloadedStrings #-}
module Prednote.Comparisons where

import Prednote.Prebuilt
import Prednote.Format
import Data.Text (Text)
import qualified Data.Text as X
import Prelude hiding (compare, not)
import qualified Prelude

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareBy
  :: Typeshow a
  -- ^ Description of the type of thing that is being matched, and how
  -- to show left-hand side values

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Ordering)
  -- ^ How to compare the left-hand side to the right-hand side.
  -- Return LT if the item is less than the right hand side; GT if
  -- greater; EQ if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Pdct a

compareBy tw rhsDesc get ord = predicate tw cond pd
  where
    cond = "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    pd a = get a == ord

-- | Overloaded version of 'compareBy'.

compare
  :: (Show a, Ord a)
  => Typedesc
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Pdct a
compare typeDesc rhs ord =
  compareBy (typeshow typeDesc) (X.pack . show $ rhs)
          (`Prelude.compare` rhs) ord

-- | Builds a 'Pred' that tests items for equality.

equalBy
  :: Typeshow a
  -- ^ Description of the type of thing that is being matched, and how
  -- to show the left-hand side

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if the items are equal; 'False' otherwise.

  -> Pdct a
equalBy tw rhsDesc = predicate tw cond
  where
    cond = "is equal to" <+> rhsDesc

-- | Overloaded version of 'equalBy'.

equal
  :: (Eq a, Show a)
  => Typedesc
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> Pdct a
equal typeDesc rhs = equalBy (typeshow typeDesc) (X.pack . show $ rhs)
  (== rhs)



-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybe
  :: Typeshow a
  -- ^ Description of the type of thing that is being matched, and how
  -- to show the left-hand side

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Maybe Ordering)
  -- ^ How to compare an item against the right hand side. Return LT if
  -- the item is less than the right hand side; GT if greater; EQ if
  -- equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Pdct a

compareByMaybe tw rhsDesc get ord = predicate tw cond pd
  where
    cond = "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    pd a = case get a of
      Nothing -> False
      Just o -> o == ord

greater
  :: (Show a, Ord a)

  => Typedesc
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pdct a
greater typeDesc rhs = compare typeDesc rhs GT

less
  :: (Show a, Ord a)

  => Typedesc
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pdct a
less typeDesc rhs = compare typeDesc rhs LT

greaterEq
  :: (Show a, Ord a)
  => Typedesc
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pdct a
greaterEq t r = greater t r ||| equal t r

lessEq
  :: (Show a, Ord a)
  => Typedesc
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pdct a
lessEq t r = less t r ||| equal t r

notEq
  :: (Show a, Eq a)
  => Typedesc
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> Pdct a
notEq t r = not $ equal t r

greaterBy
  :: Typeshow a
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pdct a
greaterBy tw desc get = compareBy tw desc get GT


lessBy
  :: Typeshow a
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pdct a
lessBy tw desc get = compareBy tw desc get LT

greaterEqBy
  :: Typeshow a
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pdct a
greaterEqBy tw desc get = greaterBy tw desc get ||| equalBy tw desc f'
  where
    f' = fmap (== EQ) get

lessEqBy
  :: Typeshow a
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pdct a
lessEqBy tw desc get = lessBy tw desc get ||| equalBy tw desc f'
  where
    f' = fmap (== EQ) get

notEqBy
  :: Typeshow a
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if equal; 'False' otherwise.

  -> Pdct a
notEqBy tw desc = not . equalBy tw desc


-- | Parses a string that contains text, such as @>=@, which indicates
-- which comparer to use.  Returns the comparer.
parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed

  -> (Ordering -> Pdct a)
  -- ^ A function that, when given an ordering, returns a 'Pdct'.
  -- Typically you will get this by partial application of 'compare',
  -- 'compareBy', or 'compareByMaybe'.

  -> Maybe (Pdct a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- 'Pdct'.
parseComparer t f
  | t == ">" = Just (f GT)
  | t == "<" = Just (f LT)
  | t == "=" = Just (f EQ)
  | t == "==" = Just (f EQ)
  | t == ">=" = Just (f GT ||| f EQ)
  | t == "<=" = Just (f LT ||| f EQ)
  | t == "/=" = Just (not $ f EQ)
  | t == "!=" = Just (not $ f EQ)
  | otherwise = Nothing

