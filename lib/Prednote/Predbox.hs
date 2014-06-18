{-# LANGUAGE OverloadedStrings #-}
module Prednote.Predbox
  (
    P.Predbox(..)

  -- * Constants
  , always
  , never

  -- * Functions and operators for conjunction and disjunction
  , P.not
  , (&&&)
  , (|||)

  -- * Fanout
  , fanand
  , fanor

  -- * Helpers for building common Predbox
  -- ** Non-overloaded

  -- | Each of these functions builds a Predbox that compares two
  -- items.  The predicate in the Predbox is applied to an item that
  -- is considered to be the left hand side of the comparison.  The
  -- left hand side side can change; the right hand side is baked
  -- into the Predbox.
  --
  -- For example, to build a Predbox that returns True if an item is
  -- greater than 5:
  --
  -- >>> :set -XOverloadedStrings
  -- >>> let p = compareBy "5" "integer" (`Prelude.compare` (5 :: Integer)) GT
  -- >>> rBool . evaluate p $ 6
  -- True
  -- >>> rBool . evaluate p $ 4
  -- False
  , compareBy
  , compareByMaybe
  , greaterBy
  , lessBy
  , equalBy
  , greaterEqBy
  , lessEqBy
  , notEqBy

  -- ** Overloaded
  , compare
  , greater
  , less
  , equal
  , greaterEq
  , lessEq
  , notEq
  , parseComparer
  ) where


import qualified Prednote.Predbox.Core as P
import Prednote.Predbox.Core ((|||), (&&&))
import System.Console.Rainbow
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid
import Prelude hiding (compare, not)
import qualified Prelude

-- | Always True
always :: P.Predbox a
always = P.Predbox (const True) (P.Predicate (P.sameLabel l) (const True))
  where
    l = ["always True"]

-- | Always False
never :: P.Predbox a
never = P.Predbox (const True) (P.Predicate (P.sameLabel l) (const False))
  where
    l = ["always False"]

-- | Creates a 'Fanand' Predbox using a generic name.
fanand
  :: (a -> [b])
  -- ^ This function is applied to every subject to derive a list of
  -- new subjects.

  -> P.Predbox b
  -- ^ This Predbox is applied to each of the new subjects.  The
  -- resulting predicate is 'True' if each of the new subjects is also
  -- True.

  -> P.Predbox a
fanand f = P.Predbox (const True) . P.Fanand (P.sameLabel l) f
  where
    l = [fromText "split into children - each child must be True"]

-- | Creates a 'Fanor' Predbox using a generic name.
fanor
  :: (a -> [b])
  -- ^ This function is applied to every subject to derive a list of
  -- new subjects.

  -> P.Predbox b
  -- ^ This Predbox is applied to each of the new subjects.  The
  -- resulting predicate is 'True' if one of the new subjects is also
  -- True.

  -> P.Predbox a
fanor f = P.Predbox (const True) . P.Fanor (P.sameLabel l) f
  where
    l = [fromText "split into children - at least one child must be True"]

-- | Build a Predbox that compares items.
compareBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare an item against the right hand side. Return LT
  -- if the item is less than the right hand side; GT if greater; EQ
  -- if equal to the right hand side.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> P.Predbox a

compareBy itemDesc typeDesc cmp ord =
  P.Predbox (const True) (P.Predicate lbl f)
  where
    l = [fromText $ typeDesc <> " is " <> cmpDesc <> " " <> itemDesc]
    cmpDesc = case ord of
      LT -> "less than"
      GT -> "greater than"
      EQ -> "equal to"
    lbl = P.sameLabel l
    f subj = cmp subj == ord

-- | Overloaded version of 'compareBy'.
compare
  :: (Show a, Ord a)
  => Text
  -- ^ Description of the type of thing being matched

  -> a
  -- ^ The right hand side of the comparison.

  -> Ordering
  -- ^ When subjects are compared, this ordering must be the result in
  -- order for the Predbox to be True; otherwise it is False. The subject
  -- will be on the left hand side.

  -> P.Predbox a
compare typeDesc a ord = compareBy itemDesc typeDesc cmp ord
  where
    itemDesc = X.pack . Prelude.show $ a
    cmp item = Prelude.compare item a

-- | Builds a Predbox for items that might fail to return a comparison.
compareByMaybe
  :: Text
  -- ^ How to show the item being compared

  -> Text
  -- ^ Description of type of thing being matched

  -> (a -> Maybe Ordering)
  -- ^ How to compare against right hand side. If Nothing, a Predbox that
  -- always returns False is returned.

  -> Ordering
  -- ^ Ordering that must result for the Predbox to be True

  -> P.Predbox a

compareByMaybe itemDesc typeDesc cmp ord =
  P.Predbox (const True) (P.Predicate l f)
  where
    lbl = fromText $ typeDesc <> " is " <> cmpDesc <> " " <> itemDesc
    l = P.sameLabel [lbl]
    cmpDesc = case ord of
      LT -> "less than"
      GT -> "greater than"
      EQ -> "equal to"
    f subj = case cmp subj of
      Nothing -> False
      Just ord' -> ord == ord'

greater
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> P.Predbox a
greater d a = compare d a GT

less
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> P.Predbox a
less d a = compare d a LT

equal
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> P.Predbox a
equal d a = compare d a EQ

greaterEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> P.Predbox a
greaterEq d a = greater d a ||| equal d a

lessEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> P.Predbox a
lessEq d a = less d a ||| equal d a

notEq
  :: (Show a, Ord a)
  => Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> a
  -- ^ The right hand side of the comparison.

  -> P.Predbox a
notEq d a = P.not $ equal d a

greaterBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> P.Predbox a
greaterBy iD tD cmp = compareBy iD tD cmp GT

lessBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> P.Predbox a
lessBy iD tD cmp = compareBy iD tD cmp LT

equalBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> P.Predbox a
equalBy iD tD cmp = compareBy iD tD cmp EQ

greaterEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> P.Predbox a
greaterEqBy iD tD cmp =
  greaterBy iD tD cmp ||| equalBy iD tD cmp

lessEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> P.Predbox a
lessEqBy iD tD cmp =
  lessBy iD tD cmp ||| equalBy iD tD cmp

notEqBy
  :: Text
  -- ^ How to show the item being compared; used to describe the Predbox

  -> Text
  -- ^ Description of the type of thing that is being matched

  -> (a -> Ordering)
  -- ^ How to compare two items

  -> P.Predbox a
notEqBy iD tD cmp =
  P.not $ equalBy iD tD cmp

-- | Parses a string to find the correct comparer; returns the correct
-- function to build a Predbox.

parseComparer
  :: Text
  -- ^ The string with the comparer to be parsed
  -> (Ordering -> P.Predbox a)
  -- ^ A function that, when given an ordering, returns a Predbox
  -> Maybe (P.Predbox a)
  -- ^ If an invalid comparer string is given, Nothing; otherwise, the
  -- Predbox.
parseComparer t f
  | t == ">" = Just (f GT)
  | t == "<" = Just (f LT)
  | t == "=" = Just (f EQ)
  | t == "==" = Just (f EQ)
  | t == ">=" = Just (f GT ||| f EQ)
  | t == "<=" = Just (f LT ||| f EQ)
  | t == "/=" = Just (P.not $ f EQ)
  | t == "!=" = Just (P.not $ f EQ)
  | otherwise = Nothing


