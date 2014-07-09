{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Core.Properties where

import Prednote.Core.Generators
import qualified Prednote.Core as C
import qualified Prednote.Prebuilt as P
import qualified Data.Tree as E
import Test.QuickCheck
import Prednote.Tests.Util
import Prelude hiding (pred)
import Control.Monad
import Test.QuickCheck.Poly (A(..))
import Data.Maybe

-- | Shown values are True
prop_shown = C.unVisible C.shown

-- | Hidden values are False
prop_hidden = not $ C.unVisible C.hidden

-- | empty list returns True for all
prop_allEmpty =
  forAll arbitrary $ \(A i) ->
  C.test (C.all []) i

-- # all

-- | list of all True preds for all returns True
prop_allTrues =
  forAll arbitrary $ \(A i) ->
  forAll (listOf1 (return P.true)) $ \ls ->
  C.test (C.all ls) i

-- | all list with at least one False pred returns False
prop_allOneFalse =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (elements [P.true, P.false])) $ \ls1 ->
  forAll (listOf (elements [P.true, P.false])) $ \ls2 ->
  not $ C.test (C.all (ls1 ++ [P.false] ++ ls2)) i

-- | all does not short circuit on True results
prop_allNoShortTrue =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.true)) $ \ls ->
  isNothing . C.short . E.rootLabel . ($ i) . C.evaluate
    $ C.all ls

-- | all does not short circuit when only the last result is False
prop_allNoShortLastFalse =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.true)) $ \ls ->
  isNothing . C.short . E.rootLabel . ($ i) . C.evaluate
    $ C.all (ls ++ [P.false])

-- | all short circuits when a result that is not the last result is
-- False
prop_allShortNotLastFalse =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (elements [P.true, P.false])) $ \ls1 ->
  forAll (listOf1 (elements [P.true, P.false])) $ \ls2 ->
  isJust . C.short . E.rootLabel . ($ i) . C.evaluate
    $ C.all (ls1 ++ [P.false] ++ ls2)

-- # any

-- | any is false on an empty list
prop_anyEmptyFalse =
  forAll arbitrary $ \(A i) ->
  not $ C.test (C.any []) i

-- | any is False on list of all False
prop_anyAllFalsesIsFalse =
  forAll arbitrary $ \(A i) ->
  forAll (listOf1 (return P.false)) $ \ls ->
  not $ C.test (C.any ls) i

-- | any is True if at least one Pred is True
prop_anyTrueOneTrue =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (elements [P.true, P.false])) $ \ls1 ->
  forAll (listOf (elements [P.true, P.false])) $ \ls2 ->
  C.test (C.any (ls1 ++ [P.true] ++ ls2)) i

-- | any does not short circuit on False results
prop_anyNoShortFalse =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.false)) $ \ls ->
  isNothing . C.short . E.rootLabel . ($ i) . C.evaluate
    $ C.any ls

-- | any does not short circuit if only the last result is True
prop_anyNoShortLastTrue =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.false)) $ \ls ->
  isNothing . C.short . E.rootLabel . ($ i) . C.evaluate
    $ C.any (ls ++ [P.true])

-- | any short circuits if any result before the last result is True
prop_anyShortsNotLastTrue =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (elements [P.true, P.false])) $ \ls1 ->
  forAll (listOf1 (elements [P.true, P.false])) $ \ls2 ->
  isJust . C.short . E.rootLabel . ($ i) . C.evaluate
    $ C.any (ls1 ++ [P.true] ++ ls2)

-- -- | Visiblity has the expected result
-- prop_visibility =
--   forAll (fmap Blind $ function1 coarbitrary visible) $ \(Blind f) ->
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   let r = E.rootLabel . ($ i) . C.evaluate . C.visibility f $ p
--   in f (C.result r) == C.visible r

-- -- | reveal always creates shown Pred
-- prop_reveal =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   (== C.shown) . C.visible . E.rootLabel . ($ i)
--     . C.evaluate . C.reveal $ p

-- -- | hide always creates hidden Pred
-- prop_hide =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   (== C.hidden) . C.visible . E.rootLabel . ($ i)
--     . C.evaluate . C.hide $ p

-- -- | showTrue shows Pred if the result is True
-- prop_showTrue =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   getResult . E.rootLabel . ($ i)
--     . C.evaluate . C.showTrue $ p
--   where
--     getResult o = C.unVisible (C.visible o) == C.result o

-- -- | showFalse shows Pred if the result is False

-- prop_showFalse =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   getResult . E.rootLabel . ($ i) . C.evaluate . C.showFalse $ p
--   where
--     getResult o = not (C.unVisible . C.visible $ o) == C.result o

-- -- | all is True on empty lists of Pred
-- prop_allIsTrueOnEmptyPred =
--   forAll (fmap Blind chunker) $ \(Blind st) ->
--   forAll (fmap Blind chunker) $ \(Blind sh) ->
--   forAll (fmap Blind dynamicLabel) $ \(Blind dyn) ->
--   forAll arbitrary $ \i ->
--   C.result . E.rootLabel . ($ i) . C.evaluate $
--     C.all st sh dyn []

