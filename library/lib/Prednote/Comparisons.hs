{-# LANGUAGE OverloadedStrings #-}
module Prednote.Comparisons where

import Prednote.Prebuilt
import Prednote.Format
import qualified Prednote.Core as C
import Data.Text (Text)
import qualified Data.Text as X
import Prelude hiding (compare, not)
import qualified Prelude

-- | Build a Pred that compares items.  The idea is that the item on
-- the right hand side is baked into the 'Pred' and that the 'Pred'
-- compares this single right-hand side to each left-hand side item.
compareBy
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare the left-hand side to the right-hand side.
  -- Return LT if the item is less than the right hand side; GT if
  -- greater; EQ if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> C.Pred a

compareBy typeDesc rhsDesc lhsDesc get ord = predicate stat dyn pd
  where
    stat = typeDesc <+> "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    dyn a = typeDesc <+> lhsDesc a <+> "is" <+> ordDesc <+> rhsDesc
    pd a = get a == ord

-- | Overloaded version of 'compareBy'.

compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> C.Pred a
compare typeDesc rhs ord =
  compareBy typeDesc (X.pack . show $ rhs) (X.pack . show)
            (`Prelude.compare` rhs) ord

-- | Builds a 'Pred' that tests items for equality.

equalBy
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if the items are equal; 'False' otherwise.

  -> C.Pred a
equalBy typeDesc rhsDesc lhsDesc get = predicate stat dyn get
  where
    stat = typeDesc <+> "is equal to" <+> rhsDesc
    dyn a = typeDesc <+> lhsDesc a <+> "is equal to" <+> rhsDesc

-- | Overloaded version of 'equalBy'.

equal
  :: (Eq a, Show a)
  => Text
  -- ^ Description of the type of thing that is being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
equal typeDesc rhs = equalBy typeDesc (X.pack . show $ rhs)
                             (X.pack . show) (== rhs)


-- | Builds a 'Pred' for items that might fail to return a comparison.
compareByMaybe
  :: Text
  -- ^ Description of the type of thing that is being matched

  -> Text
  -- ^ Description of the right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Maybe Ordering)
  -- ^ How to compare an item against the right hand side. Return LT if
  -- the item is less than the right hand side; GT if greater; EQ if
  -- equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> C.Pred a

compareByMaybe typeDesc rhsDesc lhsDesc get ord = predicate stat dyn fn
  where
    stat = typeDesc <+> "is" <+> ordDesc <+> rhsDesc
    dyn a = typeDesc <+> lhsDesc a <+> "is" <+> ordDesc <+> rhsDesc
    ordDesc = case ord of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    fn a = case get a of
      Nothing -> False
      Just o -> o == ord

greater
  :: (Show a, Ord a)

  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
greater typeDesc rhs = compare typeDesc rhs GT

less
  :: (Show a, Ord a)

  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
less typeDesc rhs = compare typeDesc rhs LT

greaterEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
greaterEq t r = greater t r ||| equal t r

lessEq
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
lessEq t r = less t r ||| equal t r

notEq
  :: (Show a, Eq a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ Right-hand side

  -> C.Pred a
notEq t r = not $ equal t r

greaterBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
greaterBy dT dR dL get = compareBy dT dR dL get GT


lessBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
lessBy dT dR dL get = compareBy dT dR dL get LT

greaterEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
greaterEqBy dT dR dL f = greaterBy dT dR dL f ||| equalBy dT dR dL f'
  where
    f' = fmap (== EQ) f

lessEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> C.Pred a
lessEqBy dT dR dL f = lessBy dT dR dL f ||| equalBy dT dR dL f'
  where
    f' = fmap (== EQ) f

notEqBy
  :: Text
  -- ^ Description of the type of thing being matched

  -> Text
  -- ^ Description of right-hand side

  -> (a -> Text)
  -- ^ Describes the left-hand side

  -> (a -> Bool)
  -- ^ How to compare an item against the right hand side.  Return
  -- 'True' if equal; 'False' otherwise.

  -> C.Pred a
notEqBy dT dR dL = not . equalBy dT dR dL


-- | Parses a string that contains text, such as @>=@, which indicates
-- which comparer to use.  Returns the comparer.
parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed

  -> (Ordering -> C.Pred a)
  -- ^ A function that, when given an ordering, returns a 'C.Pred'.
  -- Typically you will get this by partial application of 'compare',
  -- 'compareBy', or 'compareByMaybe'.

  -> Maybe (C.Pred a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- 'C.Pred'.
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

