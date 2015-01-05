{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Prebuilt.Properties where

import Prednote.Core.Generators
import Prednote.Prebuilt.Generators hiding (text)
import Prelude.Generators
import qualified Prelude.Generators as PG
import Test.QuickCheck
import Test.QuickCheck.Poly (A(..))
import Data.Text.Generators
import qualified Prednote.Prebuilt as P
import qualified Prednote.Core as C
import Prednote.Prebuilt ((&&&), (|||))
import qualified Data.Tree as E
import Prednote.Core (Pred)
import Prelude hiding (pred)
import qualified Prelude
import Data.Maybe

genInt :: Gen Int
genInt = arbitrary

prop_trueReturnsTrue =
  forAll genInt $ \i ->
  P.test P.true i

prop_falseReturnsFalse =
  forAll genInt $ \i ->
  not (P.test P.false i)

prop_sameReturnsSame =
  forAll arbitrary $ \b ->
  P.test P.same b == b

prop_predicateReturnsSame =
  forAll typedesc $ \td ->
  forAll (text arbitrary) $ \txt ->
  forAll (fmap Blind (function1 coarbitrary (text arbitrary)))
    $ \(Blind desc) ->
  forAll (fmap Blind ((function1 coarbitrary arbitrary)))
    $ \(Blind pd) ->
  forAll genInt $ \i ->
  P.test (P.predicate td txt desc pd) i == pd i

data AndOperator = AndOperator
  { andOpPd1 :: P.Pred Int
  , andOpPd2 :: P.Pred Int
  , andOpInt :: Int
  , andOpAndResult :: Bool
  , andOpAndResult1 :: Bool
  , andOpAndResult2 :: Bool
  , andOpFinalResult :: Bool
  } deriving Show

instance Arbitrary AndOperator where
  arbitrary = do
    pd1 <- pred
    pd2 <- pred
    i <- genInt
    let andResult = P.test (pd1 &&& pd2) i
        andResult1 = P.test pd1 i
        andResult2 = P.test pd2 i
        final = andResult == (andResult1 && andResult2)
    return $ AndOperator pd1 pd2 i andResult andResult1 andResult2
      final

prop_andOperator = andOpFinalResult

prop_orOperator =
  forAll pred $ \pd1 ->
  forAll pred $ \pd2 ->
  forAll genInt $ \i ->
  (P.test (pd1 ||| pd2) i) == (P.test pd1 i || P.test pd2 i)

prop_notOperator =
  forAll pred $ \pd1 ->
  forAll genInt $ \i ->
  P.test (P.not pd1) i == not (P.test pd1 i)

prop_either =
  forAll typedesc $ \td1 ->
  forAll typedesc $ \td2 ->
  forAll pred $ \pd1 ->
  forAll pred $ \pd2 ->
  forAll (PG.either genInt genInt) $ \ei ->
  let pdE = P.either td1 td2 pd1 pd2
      r = P.test pdE ei
      expected = case ei of
        Left i -> P.test pd1 i
        Right i -> P.test pd2 i
  in r == expected

prop_all =
  forAll typedesc $ \td1 ->
  forAll typedesc $ \td2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fDesc) ->
  forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind fpd) ->
  forAll (resize 2 $ listOf genInt) $ \ls ->
  forAll (text arbitrary) $ \txt ->
  let pd = P.predicate td1 txt fDesc fpd in
  within (seconds 10) $ P.test (P.all td2 pd) ls == Prelude.all fpd ls

prop_any =
  forAll typedesc $ \td1 ->
  forAll typedesc $ \td2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fDesc) ->
  forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind fpd) ->
  forAll (listOf genInt) $ \ls ->
  forAll (text arbitrary) $ \txt ->
  let pd = P.predicate td1 txt fDesc fpd in
  within (seconds 10) $ P.test (P.any td2 pd) ls == Prelude.any fpd ls

-- a :: Int; b :: Char
prop_wrap =
  let genShower = fmap Blind $ function1 coarbitrary (text arbitrary) in
  forAll typedesc $ \td1 ->
  forAll (text arbitrary) $ \txt ->
  forAll genShower $ \(Blind shw1) ->
  forAll (fmap Blind $ function1 coarbitrary arbitrary )
    $ \(Blind pd) ->
  forAll typedesc $ \td2 ->
  forAll genShower $ \(Blind shw2) ->
  forAll typedesc $ \td3 ->
  forAll genShower $ \(Blind shw3) ->
  forAll (fmap Blind $ function1 coarbitrary arbitrary)
    $ \(Blind conv) ->
  forAll genInt $ \i ->
  let p1 = P.predicate td1 txt shw1 pd 
      wrapped = P.wrap (td2, shw2) (td3, shw3) conv p1
      expected = pd . conv $ i
      actual = P.test wrapped i
  in expected == actual

seconds :: Int -> Int
seconds = (* 10 ^ (6 :: Int))

-- -- # tests for visibility
-- isShown :: Pred Int -> Property
-- isShown p =
--   forAll arbitrary $ \i ->
--   (== C.shown) . C.visible . E.rootLabel . C.evaluate p $ i

-- isHidden :: Pred Int -> Property
-- isHidden p =
--   forAll arbitrary $ \i ->
--   not . (== C.shown) . C.visible . E.rootLabel . C.evaluate p $ i

-- -- # predicate

-- -- | Predicate is same as running bare function
-- prop_predicate =
--   forAll (text arbitrary) $ \x ->
--   forAll ( fmap Blind $
--            function1 coarbitrary (text arbitrary)) $ \(Blind dyn) ->
--   forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind p) ->
--   forAll arbitrary $ \(A i) ->
--   C.test (P.predicate x dyn p) i === p i

-- prop_predicateVisible =
--   forAll (text arbitrary) $ \x ->
--   forAll ( fmap Blind $
--            function1 coarbitrary (text arbitrary)) $ \(Blind dyn) ->
--   forAll (fmap Blind $ function1 coarbitrary arbitrary) $ \(Blind p) ->
--   isShown (P.predicate x dyn p)

-- -- # true
-- prop_true =
--   forAll arbitrary $ \(A i) ->
--   C.test P.true i

-- prop_trueShown = isShown P.true

-- -- # false
-- prop_false =
--   forAll arbitrary $ \(A i) ->
--   not $ C.test P.false i

-- prop_falseShown = isShown P.false

-- -- # Wrap
-- prop_wrapResultSameAsChild =
--   forAll (text arbitrary) $ \st ->
--   forAll (fmap Blind $ function1 coarbitrary (text arbitrary)) $
--     \(Blind fDyn) ->
--   forAll (fmap Blind arbitrary) $ \(Blind xfrm) ->
--   let _types = xfrm :: Int -> Int in
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   C.test p (xfrm i) === C.test (P.wrap st fDyn xfrm p) i

-- prop_wrapVisible =
--   forAll (text arbitrary) $ \st ->
--   forAll (fmap Blind $ function1 coarbitrary (text arbitrary)) $
--     \(Blind fDyn) ->
--   forAll (fmap Blind arbitrary) $ \(Blind xfrm) ->
--   forAll pred $ \p ->
--   forAll arbitrary $ \(A i) ->
--   (== C.shown) . C.visible . E.rootLabel . ($ i) . C.evaluate $
--     P.wrap st fDyn xfrm p

-- prop_wrapNoShortCircuit =
--   forAll (text arbitrary) $ \st ->
--   forAll (fmap Blind $ function1 coarbitrary (text arbitrary)) $
--     \(Blind fDyn) ->
--   forAll (fmap Blind arbitrary) $ \(Blind xfrm) ->
--   forAll pred $ \p ->
--   forAll arbitrary $ \(A i) ->
--   isNothing . C.short . E.rootLabel . ($ i) . C.evaluate $
--     P.wrap st fDyn xfrm p

-- -- # visibility

-- prop_visibility =
--   forAll pred $ \p ->
--   forAll (fmap Blind $ function1 coarbitrary visible) $ \(Blind f) ->
--   forAll arbitrary $ \i ->
--   let r = E.rootLabel . ($ i) . C.evaluate . P.visibility f $ p in
--   C.visible r === f (C.result r)

-- prop_reveal =
--   forAll pred $ \p ->
--   isShown (P.reveal p)

-- prop_hidden =
--   forAll pred $ \p ->
--   isHidden (P.hide p)

-- prop_showTrue =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   let r = E.rootLabel . ($ i) . C.evaluate . P.showTrue $ p in
--   if C.result r then C.visible r == C.shown
--     else C.visible r == C.hidden

-- prop_showFalse =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   let r = E.rootLabel . ($ i) . C.evaluate . P.showFalse $ p in
--   if C.result r then C.visible r == C.hidden
--     else C.visible r == C.shown

-- samePredGen :: Show a => Gen a -> Pred a -> Pred a -> Property
-- samePredGen g x y =
--   forAll g $ \i ->
--   let getR = fmap (\(C.Output r v s _) -> (r, v, isNothing s))
--         . ($ i) . C.evaluate in
--   getR x === getR y
  
-- samePred :: Pred Int -> Pred Int -> Property
-- samePred = samePredGen arbitrary

-- samePredList :: Pred [Int] -> Pred [Int] -> Property
-- samePredList = samePredGen (listOf arbitrary)

-- prop_allSame =
--   forAll (listOf pred) $ \ps ->
--   samePred (C.all ps) (P.all ps)

-- prop_allOpSame =
--   forAll pred $ \p1 ->
--   forAll pred $ \p2 ->
--   samePred (P.all [p1, p2]) (p1 &&& p2)

-- prop_anySame =
--   forAll (listOf pred) $ \ps ->
--   samePred (C.any ps) (P.any ps)

-- prop_anyOpSame =
--   forAll pred $ \p1 ->
--   forAll pred $ \p2 ->
--   samePred (P.any [p1, p2]) (p1 ||| p2)

-- prop_notSame =
--   forAll pred $ \p ->
--   samePred (P.not p) (C.not p)

-- prop_fanAllSame =
--   forAll pred $ \p ->
--   samePredList (P.fanAll id p) (C.fanAll id p)

-- prop_fanAnySame =
--   forAll pred $ \p ->
--   samePredList (P.fanAny id p) (C.fanAny id p)

-- prop_fanAtLeastSame =
--   forAll pred $ \p ->
--   forAll arbitrary $ \i ->
--   samePredList (P.fanAtLeast i id p) (C.fanAtLeast i id p)
