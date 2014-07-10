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

-- | when all short circuits, the number of children is equal to the
-- leading number of True predicates plus one
prop_allShortCirLength =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.true)) $ \ls1 ->
  forAll (listOf (elements [P.true, P.false])) $ \ls2 ->
  (== (length ls1 + 1)) . length . E.subForest . ($ i) . C.evaluate
    $ C.all (ls1 ++ [P.false] ++ ls2)

-- | When all does not short circuit, the number of children is equal
-- to the number of inputs
prop_allAllTrueLength =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.true)) $ \ls1 ->
  (== (length ls1)) . length . E.subForest . ($ i) . C.evaluate
    $ C.all ls1

-- | When all does not short circuit, the number of children is equal
-- to the number of inputs
prop_allOneFalseLength =
  forAll arbitrary $ \(A i) ->
  forAll (listOf (return P.true)) $ \ls1 ->
  (== (length ls1 + 1)) . length . E.subForest . ($ i) . C.evaluate
    $ C.all (ls1 ++ [P.false])

-- | children of result of all is always a list of True followed by,
-- at most, one False.
prop_allResultSpan =
  forAll arbitrary $ \i ->
  forAll (listOf pred) $ \ls ->
  (< 2) . length . dropWhile (C.result . E.rootLabel) . E.subForest
    $ C.evaluate (C.all ls) i

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

-- | If there is a short circuit, last element of children is True
prop_anyShortCircuitLastChild =
  forAll arbitrary $ \i ->
  forAll (listOf pred) $ \ps ->
  let r = C.evaluate (C.any ps) i
  in isJust (C.short . E.rootLabel $ r)
     ==> last (map (C.result . E.rootLabel) . E.subForest $ r)

-- | children of result of any is always a list of False followed by,
-- at most, one True.
prop_anyResultSpan =
  forAll arbitrary $ \i ->
  forAll (listOf pred) $ \ls ->
  (< 2) . length . dropWhile (not . C.result . E.rootLabel) . E.subForest
    $ C.evaluate (C.any ls) i

-- # not

-- | not always has exactly one child

prop_notOneChild =
  forAll arbitrary $ \i ->
  forAll pred $ \p ->
  (== 1) . length . E.subForest $ C.evaluate (C.not p) i

-- | not never short circuits

prop_notNeverShort =
  forAll arbitrary $ \i ->
  forAll pred $ \p ->
  isNothing . C.short . E.rootLabel $ C.evaluate (C.not p) i

-- | Result of not is always opposite child

prop_notIsOpposite =
  forAll arbitrary $ \i ->
  forAll pred $ \p ->
  let r = C.evaluate (C.not p) i
  in (C.result . E.rootLabel $ r) ==
      (not . C.result . E.rootLabel . head . E.subForest $ r)

-- # fan

-- | fan always short circuits if analyzer returns a Just Int with an
-- Int less than or equal to zero

prop_fanAlwaysShortsOnNonPositive =
  forAll (oneof [return undefined, listOf (return ())]) $ \ls ->
  forAll (elements [P.true, P.false]) $ \p ->
  let genInt = fmap (negate . abs) arbitrarySizedIntegral
      genTup = liftM3 (,,) arbitrary visible (fmap Just genInt)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  isJust . C.short . E.rootLabel $
    C.evaluate (C.fan fn id p) ls

-- -- | fan does not short circuit if fanner returns a Just Int that is
-- -- greater than or equal to the length of the list.

-- prop_fanNoShortCircuit =
--   forAll (listOf (return ())) $ \ls ->
--   forAll (elements [P.true, P.false]) $ \p ->
--   let genInt = oneof [ return (length ls),
--                        choose (length ls, maxBound)]
--       genTup = liftM3 (,,) arbitrary visible (fmap Just genInt)
--       genFn = fmap Blind $ function1 coarbitrary genTup in
--   forAll genFn $ \(Blind fn) ->
--   isNothing . C.short . E.rootLabel $
--     C.evaluate (C.fan fn id p) ls

-- -- | fan does not short circuit if the input list is empty or has one
-- -- element

-- prop_fanNoShortCircuitEmptyOrOne =
--   forAll (elements [P.true, P.false]) $ \p ->
--   forAll (elements [0,1]) $ \i ->
--   let genTup = liftM3 (,,) arbitrary visible arbitrary
--       genFn = fmap Blind $ function1 coarbitrary genTup in
--   forAll genFn $ \(Blind fn) ->
--   isNothing . C.short . E.rootLabel $
--     C.evaluate (C.fan fn id p) (replicate i ())

-- -- | fan short circuits on lists at least two long if returned integer
-- -- is less than the length of the list
-- prop_fanShortCircuit =
--   forAll (elements [P.true, P.false]) $ \p ->
--   forAll (arbitrarySizedBoundedIntegral `suchThat` (> 1)) $ \i ->
--   let genInt = oneof [ return 0, return (i - 1),
--                        choose (minBound, i - 1) ]
--       genTup = liftM3 (,,) arbitrary visible (fmap Just genInt)
--       genFn = fmap Blind $ function1 coarbitrary genTup in
--   forAll genFn $ \(Blind fn) ->
--   isNothing . C.short . E.rootLabel $
--     C.evaluate (C.fan fn id p) (replicate i ())

