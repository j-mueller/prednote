{-# LANGUAGE OverloadedStrings, BangPatterns #-}

-- | Functions to work with 'Pred'.  This module works with 'Text' and
-- produces 'Pred' that make sparing use of color.  For more control
-- over the 'Pred' produced, use "Prednote.Pred.Core".
--
-- Exports some names that conflict with Prelude names, so you might
-- want to do something like
--
-- > import qualified Prednote.Pred as P
module Prednote.Prebuilt where

import qualified Prednote.Core as C
import Prednote.Format
import qualified Data.Tree as E
import qualified Data.Text as X
import Data.Text (Text)
import Data.Monoid
import Prelude hiding (and, or, not, filter, compare, any, all)
import qualified Prelude

-- # Predicate

-- | Builds predicates.
predicate
  :: Text
  -- ^ Static label

  -> (a -> Text)
  -- ^ Computes the dynamic label.  Do not indicate whether the result
  -- is 'True' or 'False'; this is automatically done for you.

  -> (a -> Bool)
  -- ^ Predicate function

  -> C.Pred a

predicate r s f = rename r . speak s $ C.Pred (E.Node (const []) []) ev
  where
    ev a = E.Node (C.Output (f a) C.shown Nothing (const [])) []

-- | Always returns 'True' and is always visible.
true :: C.Pred a
true = predicate l (const l) (const True)
  where
    l = "always True"

-- | Always returns 'False' and is always visible.
false :: C.Pred a
false = predicate l (const l) (const False)
  where
    l = "always False"

-- | Returns the subject as is; is always visible.
same :: C.Pred Bool
same = predicate l (const l) id
  where
    l = "same as subject"

-- # Visibility

-- | Creates a 'C.Pred' with its visibility modified.
visibility
  :: (Bool -> C.Visible)
  -- ^ When applied to the 'C.result' of the 'C.Pred', this function
  -- returns the desired visibility.
  -> C.Pred a
  -> C.Pred a
visibility f (C.Pred s e) = C.Pred s e'
  where
    e' a = g (e a)
    g (E.Node n cs) = E.Node n { C.visible = f (C.result n) } cs

-- | Creates a 'C.Pred' that is always shown.
reveal :: C.Pred a -> C.Pred a
reveal = visibility (const C.shown)

-- | Creates a 'C.Pred' that is always hidden.
hide :: C.Pred a -> C.Pred a
hide = visibility (const C.hidden)

-- | Creates a 'C.Pred' that is shown only if its 'C.result' is
-- 'True'.
showTrue :: C.Pred a -> C.Pred a
showTrue = visibility (\b -> if b then C.shown else C.hidden)

-- | Creates a 'C.Pred' that is shown only if its 'C.result' is
-- 'False'.
showFalse :: C.Pred a -> C.Pred a
showFalse = visibility (\b -> if Prelude.not b then C.shown else C.hidden)

-- # Conjunction and disjunction, negation

-- | No child 'Pred' may be 'False'.  An empty list of child 'Pred'
-- returns 'True'.  Always visible.
all :: [C.Pred a] -> C.Pred a
all = speakShort . rename l . speak (const l) . C.all
  where
    l = "all"

-- | Creates 'all' 'Pred' that are always visible.
(&&&) :: C.Pred a -> C.Pred a -> C.Pred a
l &&& r = all [l, r]

infixr 3 &&&

-- | At least one child 'Pred' must be 'True'.  An empty list of child
-- 'Pred' returns 'False'.  Always visible.
any :: [C.Pred a] -> C.Pred a
any = speakShort . rename l . speak (const l) . C.any
  where
    l = "any"


-- | Creates 'any' 'Pred' that are always visible.
(|||) :: C.Pred a -> C.Pred a -> C.Pred a
l ||| r = any [l, r]

infixr 2 |||

-- | Negation.  Always visible.
not :: C.Pred a -> C.Pred a
not = rename l . speak (const l) . C.not
  where
    l = "not"

-- | No fanned-out item may be 'False'.  An empty list of child items
-- returns 'True'.
fanAll :: (a -> [b]) -> C.Pred b -> C.Pred a
fanAll f = speakShort . rename l . speak (const l) . C.fanAll f
  where
    l = "fanout all"



-- | At least one fanned-out item must be 'True'.  An empty list of
-- child items returns 'False'.
fanAny :: (a -> [b]) -> C.Pred b -> C.Pred a
fanAny f = speakShort . rename l . speak (const l) . C.fanAny f
  where
    l = "fanout any"


-- | At least the given number of child items must be 'True'.
fanAtLeast :: Int -> (a -> [b]) -> C.Pred b -> C.Pred a
fanAtLeast i f = speakShort . rename l . speak (const l)
  . C.fanAtLeast i f
  where
    l = "fanout - at least " <> X.pack (show i) <>
      " fanned-out subject(s) must be True"

