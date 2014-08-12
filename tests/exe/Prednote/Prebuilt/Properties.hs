{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Prebuilt.Properties where

import Prednote.Core.Generators
import Prelude.Generators
import Test.QuickCheck
import Test.QuickCheck.Poly (A(..))
import Data.Text.Generators
import qualified Prednote.Prebuilt as P
import qualified Prednote.Core as C
import Prednote.Prebuilt ((&&&), (|||))
import qualified Data.Tree as E
import Prednote.Core (Pred)
import Prelude hiding (pred)
import Data.Maybe

-- # tests for visibility
isShown :: Pred Int -> Property
isShown p =
  forAll arbitrary $ \i ->
  (== C.shown) . C.visible . E.rootLabel . C.evaluate p $ i

isHidden :: Pred Int -> Property
isHidden p =
  forAll arbitrary $ \i ->
  not . (== C.shown) . C.visible . E.rootLabel . C.evaluate p $ i

-- # predicate

-- | Predicate is same as running bare function
prop_predicate =
  forAll (text arbitrary) $ \x ->
  forAll ( fmap Blind $
           function1 coarbitrary (text arbitrary)) $ \(Blind dyn) ->
  forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind p) ->
  forAll arbitrary $ \(A i) ->
  C.test (P.predicate x dyn p) i === p i

prop_predicateVisible =
  forAll (text arbitrary) $ \x ->
  forAll ( fmap Blind $
           function1 coarbitrary (text arbitrary)) $ \(Blind dyn) ->
  forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind p) ->
  isShown (P.predicate x dyn p)

-- # true
prop_true =
  forAll arbitrary $ \(A i) ->
  C.test P.true i

prop_trueShown = isShown P.true

-- # false
prop_false =
  forAll arbitrary $ \(A i) ->
  not $ C.test P.false i

prop_falseShown = isShown P.false

-- # Wrap
prop_wrapResultSameAsChild =
  forAll (text arbitrary) $ \st ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary)) $
    \(Blind fDyn) ->
  forAll (fmap Blind arbitrary) $ \(Blind xfrm) ->
  let _types = xfrm :: Int -> Int in
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  C.test p (xfrm i) === C.test (P.wrap st fDyn xfrm p) i

prop_wrapVisible =
  forAll (text arbitrary) $ \st ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary)) $
    \(Blind fDyn) ->
  forAll (fmap Blind arbitrary) $ \(Blind xfrm) ->
  forAll pred $ \p ->
  forAll arbitrary $ \(A i) ->
  (== C.shown) . C.visible . E.rootLabel . ($ i) . C.evaluate $
    P.wrap st fDyn xfrm p

prop_wrapNoShortCircuit =
  forAll (text arbitrary) $ \st ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary)) $
    \(Blind fDyn) ->
  forAll (fmap Blind arbitrary) $ \(Blind xfrm) ->
  forAll pred $ \p ->
  forAll arbitrary $ \(A i) ->
  isNothing . C.short . E.rootLabel . ($ i) . C.evaluate $
    P.wrap st fDyn xfrm p

-- # visibility

prop_visibility =
  forAll pred $ \p ->
  forAll (fmap Blind $ function1 coarbitrary visible) $ \(Blind f) ->
  forAll arbitrary $ \i ->
  let r = E.rootLabel . ($ i) . C.evaluate . P.visibility f $ p in
  C.visible r === f (C.result r)

prop_reveal =
  forAll pred $ \p ->
  isShown (P.reveal p)

prop_hidden =
  forAll pred $ \p ->
  isHidden (P.hide p)

prop_showTrue =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  let r = E.rootLabel . ($ i) . C.evaluate . P.showTrue $ p in
  if C.result r then C.visible r == C.shown
    else C.visible r == C.hidden

prop_showFalse =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  let r = E.rootLabel . ($ i) . C.evaluate . P.showFalse $ p in
  if C.result r then C.visible r == C.hidden
    else C.visible r == C.shown

samePredGen :: Show a => Gen a -> Pred a -> Pred a -> Property
samePredGen g x y =
  forAll g $ \i ->
  let getR = fmap (\(C.Output r v s _) -> (r, v, isNothing s))
        . ($ i) . C.evaluate in
  getR x === getR y
  
samePred :: Pred Int -> Pred Int -> Property
samePred = samePredGen arbitrary

samePredList :: Pred [Int] -> Pred [Int] -> Property
samePredList = samePredGen (listOf arbitrary)

prop_allSame =
  forAll (listOf pred) $ \ps ->
  samePred (C.all ps) (P.all ps)

prop_allOpSame =
  forAll pred $ \p1 ->
  forAll pred $ \p2 ->
  samePred (P.all [p1, p2]) (p1 &&& p2)

prop_anySame =
  forAll (listOf pred) $ \ps ->
  samePred (C.any ps) (P.any ps)

prop_anyOpSame =
  forAll pred $ \p1 ->
  forAll pred $ \p2 ->
  samePred (P.any [p1, p2]) (p1 ||| p2)

prop_notSame =
  forAll pred $ \p ->
  samePred (P.not p) (C.not p)

prop_fanAllSame =
  forAll pred $ \p ->
  samePredList (P.fanAll id p) (C.fanAll id p)

prop_fanAnySame =
  forAll pred $ \p ->
  samePredList (P.fanAny id p) (C.fanAny id p)

prop_fanAtLeastSame =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  samePredList (P.fanAtLeast i id p) (C.fanAtLeast i id p)
