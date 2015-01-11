{-# LANGUAGE OverloadedStrings #-}
module Prednote.Comparisons where

import Prednote.Core
import Prednote.Format
import Prelude hiding (compare, not)
import qualified Prelude

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareBy
  :: Show a
  => String
  -- ^ Description of the right-hand side

  -> (a -> Ordering)
  -- ^ How to compare the left-hand side to the right-hand side.
  -- Return LT if the item is less than the right hand side; GT if
  -- greater; EQ if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Pred a

compareBy rhsDesc get ord = predicate "" cond pd
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
  => a
  -- ^ Right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Pred a
compare rhs ord =
  compareBy (show rhs) (`Prelude.compare` rhs) ord

-- | Builds a 'Pred' that tests items for equality.

equalBy
  :: Show a

  => String
  -- ^ Description of the right-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if the items are equal; 'False' otherwise.

  -> Pred a
equalBy rhsDesc = predicate "" cond
  where
    cond = "is equal to" <+> rhsDesc

-- | Overloaded version of 'equalBy'.

equal
  :: (Eq a, Show a)
  => a
  -- ^ Right-hand side

  -> Pred a
equal rhs = equalBy (show rhs) (== rhs)


-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybe
  :: Show a
  => String
  -- ^ Description of the right-hand side

  -> (a -> Maybe Ordering)
  -- ^ How to compare an item against the right hand side. Return LT if
  -- the item is less than the right hand side; GT if greater; EQ if
  -- equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> Pred a

compareByMaybe rhsDesc get ord = predicate "" cond pd
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

  => a
  -- ^ Right-hand side

  -> Pred a
greater rhs = compare rhs GT

less
  :: (Show a, Ord a)

  => a
  -- ^ Right-hand side

  -> Pred a
less rhs = compare rhs LT

greaterEq
  :: (Show a, Ord a)
  => a
  -- ^ Right-hand side

  -> Pred a
greaterEq r = greater r ||| equal r

lessEq
  :: (Show a, Ord a)
  => a
  -- ^ Right-hand side

  -> Pred a
lessEq r = less r ||| equal r

notEq
  :: (Show a, Eq a)
  => a
  -- ^ Right-hand side

  -> Pred a
notEq = not . equal

greaterBy
  :: Show a
  => String
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
greaterBy desc get = compareBy desc get GT


lessBy
  :: Show a
  => String
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
lessBy desc get = compareBy desc get LT

greaterEqBy
  :: Show a
  => String
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
greaterEqBy desc get = greaterBy desc get ||| equalBy desc f'
  where
    f' = fmap (== EQ) get

lessEqBy
  :: Show a
  => String
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
lessEqBy desc get = lessBy desc get ||| equalBy desc f'
  where
    f' = fmap (== EQ) get

notEqBy
  :: Show a
  => String
  -- ^ Description of right-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if equal; 'False' otherwise.

  -> Pred a
notEqBy desc = not . equalBy desc


-- | Parses a string that contains text, such as @>=@, which indicates
-- which comparer to use.  Returns the comparer.
parseComparer
  :: String
  -- ^ The string with the comparer to be parsed

  -> (Ordering -> Pred a)
  -- ^ A function that, when given an ordering, returns a 'Pred'.
  -- Typically you will get this by partial application of 'compare',
  -- 'compareBy', or 'compareByMaybe'.

  -> Maybe (Pred a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- 'Pred'.
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

