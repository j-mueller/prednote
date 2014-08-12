{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Core.Properties where

import Prednote.Core.Generators
import qualified Prednote.Core as C
import qualified Prednote.Prebuilt as P
import qualified Data.Tree as E
import Test.QuickCheck
import Prelude.Generators
import Prelude hiding (pred)
import Control.Monad
import Test.QuickCheck.Poly (A(..))
import Data.Maybe
import Data.List (intersperse)

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

-- | fan does not fail on an undefined list if the analyzer does not
-- look at the list

prop_fanSucceedOnUndefined =
  forAll (return (undefined :: [Int])) $ \ls ->
  forAll (elements [P.true, P.false]) $ \p ->
  let genTup = liftM3 (,,) arbitrary visible arbitrary in
  forAll genTup $ \tup ->
  (C.result . E.rootLabel $
        C.evaluate (C.fan (const tup) id p) ls) `seq` True

-- | fan always short circuits if analyzer returns a Just Int with an
-- Int less than zero

prop_fanAlwaysShortsOnNonPositive =
  forAll (listOf (return ())) $ \ls ->
  forAll (elements [P.true, P.false]) $ \p ->
  let genInt = arbitrarySizedIntegral `suchThat` (< 0)
      genTup = liftM3 (,,) arbitrary visible (fmap Just genInt) in
  forAll genTup $ \tup ->
  isJust . C.short . E.rootLabel $
    C.evaluate (C.fan (const tup) id p) ls

-- | fan does not short circuit if analyzer returns a Just Int that is
-- greater than or equal to the length of the list.

prop_fanNoShortCircuit =
  forAll (listOf (return ())) $ \ls ->
  forAll (elements [P.true, P.false]) $ \p ->
  let genInt = oneof [ return (length ls),
                       choose (length ls, maxBound)]
      genTup = liftM3 (,,) arbitrary visible (fmap Just genInt)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  isNothing . C.short . E.rootLabel $
    C.evaluate (C.fan fn id p) ls

-- | fan short circuits if analyzer returns a Just Int that is less
-- than the length of the list

prop_fanShortCircuitIntLessThan =
  forAll (listOf (return ())) $ \ls ->
  forAll (elements [P.true, P.false]) $ \p ->
  let genInt = oneof [ return (length ls - 1),
                       choose (minBound, length ls - 1) ]
      genTup = liftM3 (,,) arbitrary visible (fmap Just genInt)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  isJust . C.short . E.rootLabel
    $ C.evaluate (C.fan fn id p) ls

-- | fan never short circuits if analyzer returns Nothing

prop_fanNoShortCircuitOnNothing =
  forAll (listOf (return ())) $ \ls ->
  forAll (elements [P.true, P.false]) $ \p ->
  let genTup = liftM3 (,,) arbitrary visible (return Nothing)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  isNothing . C.short . E.rootLabel
    $ C.evaluate (C.fan fn id p) ls

-- | fan result is as expected

prop_fanResult =
  forAll (listOf arbitrary) $ \ls ->
  forAll pred $ \p ->
  let genTup = liftM3 (,,) arbitrary visible (return Nothing)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  let bs = map (C.test p) ls
      r = C.test (C.fan fn id p) ls
      (rFn, _, _) = fn bs
  in r === rFn

-- | fan visibility is as expected
prop_fanVisibility =
  forAll (listOf arbitrary) $ \ls ->
  forAll pred $ \p ->
  let genTup = liftM3 (,,) arbitrary visible (return Nothing)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  let bs = map (C.test p) ls
      r = C.visible . E.rootLabel $ C.evaluate (C.fan fn id p) ls
      (_, rFn, _) = fn bs
  in r === rFn

-- | number of fan children is either the number of fanout items or
-- the number indicated by the short circuit, whichever is less
prop_fanNumberOfChildren =
  forAll (listOf arbitrary) $ \ls ->
  forAll pred $ \p ->
  let genTup = liftM3 (,,) arbitrary visible (return Nothing)
      genFn = fmap Blind $ function1 coarbitrary genTup in
  forAll genFn $ \(Blind fn) ->
  let bs = map (C.test p) ls
      r = E.subForest $ C.evaluate (C.fan fn id p) ls
      (_, _, may) = fn bs in
  case may of
    Nothing -> length r == length ls
    Just i -> length r == (max 0 (min i (length ls)))

-- # fanAll

-- | fanAll is True on empty list

prop_fanAllTrueOnEmptyList =
  forAll pred $ \p ->
  C.test (C.fanAll (const []) p) []

-- | fanAll is True if all children are True
prop_fanAllTrueAllItemsTrue =
  forAll (listOf1 (return True)) $ \ls ->
  C.test (C.fanAll id P.same) ls

-- | fanAll is False if at least one value is False
prop_fanAllFalseOneItemFalse =
  forAll (listOf arbitrary) $ \ls1 ->
  forAll (listOf arbitrary) $ \ls2 ->
  not $ C.test (C.fanAll id P.same) (ls1 ++ [False] ++ ls2)

-- | fanAll does not short circuit if result is True
prop_fanAllNoShortCircuitTrue =
  forAll (listOf (return True)) $ \ls ->
  isNothing . C.short . E.rootLabel $
    C.evaluate (C.fanAll id P.same) ls

-- | fanAll does not short circuit if only the last value is False
prop_fanAllNoShortCircuitLastFalse =
  forAll (listOf (return True)) $ \ls ->
  isNothing . C.short . E.rootLabel $
    C.evaluate (C.fanAll id P.same) (ls ++ [False])

-- | fanAll does short circuit if there is a False before the last
-- value
prop_fanAllShortCircuitFalseNotLast =
  forAll (listOf arbitrary) $ \ls1 ->
  forAll (listOf1 arbitrary) $ \ls2 ->
  isJust . C.short . E.rootLabel $
    C.evaluate (C.fanAll id P.same) (ls1 ++ [False] ++ ls2)

-- | In short circuit, length of resulting items is length of list up
-- to first False

prop_fanAllShortCircuitLength =
  forAll (listOf (return True)) $ \ls1 ->
  forAll (listOf1 arbitrary) $ \ls2 ->
  (== (length ls1 + 1)) . length . E.subForest $
    C.evaluate (C.fanAll id P.same) (ls1 ++ [False] ++ ls2)

-- # fanAny

-- | fanAny is False on empty list

prop_fanAnyFalseOnEmptyList =
  forAll pred $ \p ->
  not $ C.test (C.fanAny (const []) p) []

-- | fanAny is False if all children are False
prop_fanAnyFalseAllItemsFalse =
  forAll (listOf1 (return False)) $ \ls ->
  not $ C.test (C.fanAny id P.same) ls

-- | fanAny is True if at least one value is True
prop_fanAnyFalseOneItemFalse =
  forAll (listOf arbitrary) $ \ls1 ->
  forAll (listOf arbitrary) $ \ls2 ->
  C.test (C.fanAny id P.same) (ls1 ++ [True] ++ ls2)

-- | fanAny does not short circuit if result is False
prop_fanAnyNoShortCircuitFalse =
  forAll (listOf (return False)) $ \ls ->
  isNothing . C.short . E.rootLabel $
    C.evaluate (C.fanAny id P.same) ls

-- | fanAny does not short circuit if only the last value is True
prop_fanAnyNoShortCircuitLastTrue =
  forAll (listOf (return False)) $ \ls ->
  isNothing . C.short . E.rootLabel $
    C.evaluate (C.fanAny id P.same) (ls ++ [True])

-- | fanAny does short circuit if there is a True before the last
-- value
prop_fanAnyShortCircuitFalseNotLast =
  forAll (listOf arbitrary) $ \ls1 ->
  forAll (listOf1 arbitrary) $ \ls2 ->
  isJust . C.short . E.rootLabel $
    C.evaluate (C.fanAny id P.same) (ls1 ++ [True] ++ ls2)

-- | In short circuit, length of resulting items is length of list up
-- to first True

prop_fanAnyShortCircuitLength =
  forAll (listOf (return False)) $ \ls1 ->
  forAll (listOf1 arbitrary) $ \ls2 ->
  (== (length ls1 + 1)) . length . E.subForest $
    C.evaluate (C.fanAny id P.same) (ls1 ++ [True] ++ ls2)

-- # fanAtLeast

-- | Always returns True if argument is less than or equal to zero.
prop_fanAtLeastLTEQZero =
  forAll pred $ \p ->
  forAll (fmap (negate . abs) arbitrary) $ \i ->
  forAll (listOf arbitrary) $ \ls ->
  C.test (C.fanAtLeast i id p) ls

surround :: a -> [a] -> [a]
surround a = (a :) . intersperse a . (++ [a])

-- | Generates a list of the given number of True items, surrounded by
-- a random number of False items.
listOfTrue :: Int -> Gen [Bool]
listOfTrue i = fmap concat . sequence $ surround (listOf (return False))
  (replicate i (return [True]))

-- | Returns True if requisite number of items is found.

prop_fanAtLeastTrue =
  forAll (fmap getPositive arbitrary) $ \i ->
  forAll (listOfTrue i) $ \ls ->
  forAll (oneof [return i, choose (0, i)]) $ \n ->
  C.test (C.fanAtLeast n id P.same) ls

-- | Short circuits if fewer matching items are requested than are
-- present.

prop_fanAtLeastShortFewer =
  forAll (fmap getPositive arbitrary) $ \i ->
  forAll (listOfTrue i) $ \ls ->
  forAll (oneof [return (i - 1), choose (0, i - 1)]) $ \n ->
  isJust . C.short . E.rootLabel $
    C.evaluate (C.fanAtLeast n id P.same) ls

-- | Returns False if given number of items is not present.

prop_fanAtLeastFalse =
  forAll (fmap getNonNegative arbitrary) $ \i ->
  forAll (listOfTrue i) $ \ls ->
  forAll (oneof [return (i + 1), choose (i + 1, maxBound)]) $ \n ->
  not $ C.test (C.fanAtLeast n id P.same) ls

-- | Short circuits if additional items are after number of found items.
prop_fanAtLeastShortMoreAreAfter =
  forAll (fmap getPositive arbitrary) $ \i ->
  forAll (listOfTrue i) $ \ls ->
  forAll (listOf1 arbitrary) $ \more ->
  forAll (choose (0, i)) $ \n ->
  isJust . C.short . E.rootLabel
    $ C.evaluate (C.fanAtLeast n id P.same) (ls ++ more)

-- # report, plan - tested visually

-- | test is same as evaluation
prop_testIsEvaluation =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  C.test p i == C.result (E.rootLabel $ C.evaluate p i)

-- | fst . testV is the same as test

prop_testVSameAsTest =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  fst (C.testV p i) == C.result (E.rootLabel $ C.evaluate p i)

-- | snd . testV is same as report

prop_testVSameAsReport =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  snd (C.testV p i) == C.report 0 (C.evaluate p i)

-- # filter

-- | filter is the same as filter

prop_filter =
  forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind fn) ->
  forAll visible $ \v ->
  forAll (fmap Blind $ (frequency [ (3, fmap Just chunker),
                      (1, return Nothing)])) $ \(Blind mayCkr) ->
  forAll (fmap Blind chunker) $ \(Blind ckr) ->
  forAll (listOf arbitrary) $ \ls ->
  let p = C.Pred (E.Node (const []) [])
        (\a -> E.Node (C.Output (fn (a :: Int)) v mayCkr ckr) []) in
  C.filter p ls === filter fn ls

-- | fst . filterV is the same as filter

prop_filterVFst =
  forAll pred $ \p ->
  forAll arbitrary $ \ls ->
  fst (C.filterV p ls) === C.filter p ls

-- | snd . filterV is the same as reporting on each item
prop_filterVSnd =
  forAll pred $ \p ->
  forAll arbitrary $ \ls ->
  snd (C.filterV p ls) ===
    concat (map snd . map (C.testV p) $ ls)

-- # shorter

-- | shorter does what it should

prop_shorter =
  forAll (listOf (return ())) $ \ls1 ->
  forAll (listOf (return ())) $ \ls2 ->
  ls1 `C.shorter` ls2 === (length ls1 < length ls2)

-- | shorter is OK with longer list that is undefined

prop_shorterLazy =
  forAll (listOf (return ())) $ \ls1 ->
  let ls2 = ls1 ++ (() : undefined)
  in ls1 `C.shorter` ls2
  
