{-# LANGUAGE OverloadedStrings #-}
module Prednote.Comparisons
  ( -- * Comparisions that do not run in a context
    compareBy
  , compare
  , equalBy
  , equal
  , compareByMaybe
  , greater
  , less
  , greaterEq
  , lessEq
  , notEq
  , greaterBy
  , lessBy
  , greaterEqBy
  , lessEqBy
  , notEqBy

  -- * Comparisions that run in a context
  , compareByM
  , equalByM
  , compareByMaybeM
  , greaterByM
  , lessByM
  , greaterEqByM
  , lessEqByM
  , notEqByM

  -- * Parsing comparers
  , parseComparer
  ) where

import Prednote.Core
import Prelude hiding (compare, not)
import qualified Prelude
import Data.Monoid
import qualified Data.Text as X
import Data.Text (Text)
import Data.String
import Rainbow

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareByM
  :: (Show a, Functor f)
  => Text
  -- ^ Description of the right-hand side

  -> (a -> f Ordering)
  -- ^ How to compare the left-hand side to the right-hand side.
  -- Return LT if the item is less than the right hand side; GT if
  -- greater; EQ if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> PredM f a

compareByM rhsDesc get tgt = predicateM f
  where
    f a = fmap mkTup (get a)
      where
        mkTup ord = (bl, val, cond)
          where
            val = Value [fromString . show $ a]
            cond = Condition [fromText condTxt]
            condTxt = "is" <+> ordDesc <+> rhsDesc
            ordDesc = case ord of
              EQ -> "equal to"
              LT -> "less than"
              GT -> "greater than"
            bl = ord == tgt

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareBy
  :: Show a
  => Text
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

compareBy rhsDesc get ord = compareByM rhsDesc (fmap return get) ord

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
  compareBy (X.pack . show $ rhs) (`Prelude.compare` rhs) ord

-- | Builds a 'Pred' that tests items for equality.

equalByM
  :: (Show a, Functor f)

  => Text
  -- ^ Description of the right-hand side

  -> (a -> f Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if the items are equal; 'False' otherwise.

  -> PredM f a
equalByM rhsDesc get = predicateM f
  where
    f a = fmap mkTup (get a)
      where
        mkTup bl = (bl, Value [fromString . show $ a],
          Condition [fromText $ "is equal to" <+> rhsDesc])

-- | Builds a 'Pred' that tests items for equality.

equalBy
  :: Show a

  => Text
  -- ^ Description of the right-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if the items are equal; 'False' otherwise.

  -> Pred a
equalBy rhsDesc f = equalByM rhsDesc (fmap return f)

-- | Overloaded version of 'equalBy'.

equal
  :: (Eq a, Show a)
  => a
  -- ^ Right-hand side

  -> Pred a
equal rhs = equalBy (X.pack . show $ rhs) (== rhs)


-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybeM
  :: (Functor f, Show a)
  => Text
  -- ^ Description of the right-hand side

  -> (a -> f (Maybe Ordering))
  -- ^ How to compare an item against the right hand side. Return LT if
  -- the item is less than the right hand side; GT if greater; EQ if
  -- equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> PredM f a

compareByMaybeM rhsDesc get ord = predicateM f
  where
    f a = fmap mkTup (get a)
      where
        mkTup mayOrd = (bl, val, cond)
          where
            val = Value [fromString . show $ a]
            cond = Condition [fromText $ "is" <+> ordDesc <+> rhsDesc]
            ordDesc = case ord of
              EQ -> "equal to"
              LT -> "less than"
              GT -> "greater than"
            bl = case mayOrd of
              Nothing -> False
              Just o -> o == ord


-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybe
  :: Show a
  => Text
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

compareByMaybe rhsDesc get ord = compareByMaybeM rhsDesc (fmap return get) ord

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

greaterByM
  :: (Show a, Functor f)
  => Text
  -- ^ Description of right-hand side

  -> (a -> f Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> PredM f a
greaterByM desc get = compareByM desc get GT

greaterBy
  :: Show a
  => Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
greaterBy desc get = greaterByM desc (fmap return get)


lessByM
  :: (Show a, Functor f)
  => Text
  -- ^ Description of right-hand side

  -> (a -> f Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> PredM f a
lessByM desc get = compareByM desc get LT

lessBy
  :: Show a
  => Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
lessBy desc get = lessByM desc (fmap return get)

greaterEqByM
  :: (Functor f, Monad f, Show a)
  => Text
  -- ^ Description of right-hand side

  -> (a -> f Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> PredM f a
greaterEqByM desc get = greaterByM desc get ||| equalByM desc f'
  where
    f' = fmap (fmap (== EQ)) get

greaterEqBy
  :: Show a
  => Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
greaterEqBy desc get = greaterEqByM desc (fmap return get)

lessEqByM
  :: (Functor f, Monad f, Show a)
  => Text
  -- ^ Description of right-hand side

  -> (a -> f Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> PredM f a
lessEqByM desc get = lessByM desc get ||| equalByM desc f'
  where
    f' = fmap (fmap (== EQ)) get

lessEqBy
  :: Show a
  => Text
  -- ^ Description of right-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Pred a
lessEqBy desc get = lessEqByM desc (fmap return get)

notEqByM
  :: (Functor f, Show a)
  => Text
  -- ^ Description of right-hand side

  -> (a -> f Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if equal; 'False' otherwise.

  -> PredM f a
notEqByM desc = not . equalByM desc

notEqBy
  :: Show a
  => Text
  -- ^ Description of right-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if equal; 'False' otherwise.

  -> Pred a
notEqBy desc f = notEqByM desc (fmap return f)

-- | Parses a string that contains text, such as @>=@, which indicates
-- which comparer to use.  Returns the comparer.
parseComparer
  :: (Monad f, Functor f)
  => Text
  -- ^ The string with the comparer to be parsed

  -> (Ordering -> PredM f a)
  -- ^ A function that, when given an ordering, returns a 'Pred'.
  -- Typically you will get this by partial application of 'compare',
  -- 'compareBy', or 'compareByMaybe'.

  -> Maybe (PredM f a)
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

-- | Append two 'X.Text', with an intervening space if both 'X.Text'
-- are not empty.
(<+>) :: Text -> Text -> Text
l <+> r
  | full l && full r = l <> " " <> r
  | otherwise = l <> r
  where
    full = Prelude.not . X.null

