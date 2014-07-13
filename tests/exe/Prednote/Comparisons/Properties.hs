{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Prednote.Comparisons.Properties where

import Data.Text.Generators
import Test.QuickCheck
import Data.Text (Text)
import qualified Prednote.Comparisons as M
import qualified Prednote.Core as C
import Prednote.Tests.Util

-- | compareBy is same as compare
prop_compareBy =
  forAll (text arbitrary) $ \t1 ->
  forAll (text arbitrary) $ \t2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fTxt) ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  let _types = (iLeft, iRight) :: (Int, Int) in
  forAll arbitrary $ \ord ->
  forAll (fmap Blind $ function2 coarbitrary coarbitrary arbitrary)
    $ \(Blind fCmp) ->
  C.test (M.compareBy t1 t2 fTxt (`fCmp` iRight) ord) iLeft
    == (fCmp iLeft iRight == ord)

-- | compare behaves like Prelude.compare
prop_compare =
  forAll (text arbitrary) $ \t1 ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  let _types = (iLeft, iRight) :: (Int, Int) in
  forAll arbitrary $ \ord ->
  C.test (M.compare t1 iRight ord) iLeft
    == (compare iLeft iRight == ord)

-- | equalBy works as expected

prop_equalBy =
  forAll (text arbitrary) $ \t1 ->
  forAll (text arbitrary) $ \t2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fTxt) ->
  forAll (fmap Blind $ function2 coarbitrary coarbitrary arbitrary)
    $ \(Blind p) ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  let _types = (iLeft, iRight) :: (Int, Int) in
  C.test (M.equalBy t1 t2 fTxt (`p` iRight)) iLeft
    == (p iLeft iRight)

-- | equal works as expected

prop_equal =
  forAll (text arbitrary) $ \t1 ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  let _types = (iLeft, iRight) :: (Int, Int) in
  C.test (M.equal t1 iRight) iLeft
    == (iLeft == iRight)

-- | compareByMaybe works as it should
prop_compareByMaybe =
  forAll (text arbitrary) $ \t1 ->
  forAll (text arbitrary) $ \t2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fTxt) ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  let _types = (iLeft, iRight) :: (Int, Int) in
  forAll arbitrary $ \ord ->
  forAll (fmap Blind $ function2 coarbitrary coarbitrary arbitrary)
    $ \(Blind fCmp) ->
  let rPred = C.test (M.compareByMaybe t1 t2 fTxt (`fCmp` iRight) ord)
        iLeft
      rCmp = fCmp iLeft iRight
      r = case rCmp of
        Nothing -> False
        Just o -> o == ord
  in rPred == r

testOverloadedFn
  :: (Text -> Int -> C.Pred Int)
  -> (Int -> Int -> Bool)
  -> Property
testOverloadedFn f fb =
  forAll (text arbitrary) $ \txt ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  C.test (f txt iRight) iLeft == fb iLeft iRight

prop_greater = testOverloadedFn M.greater (>)
prop_less = testOverloadedFn M.less (<)
prop_greaterEq = testOverloadedFn M.greaterEq (>=)
prop_lessEq = testOverloadedFn M.lessEq (<=)
prop_notEq = testOverloadedFn M.notEq (/=)

type ByFn
  = Text
  -> Text
  -> (Int -> Text)
  -> (Int -> Ordering)
  -> C.Pred Int

testByFn :: ByFn -> (Int -> Int -> Bool) -> Property
testByFn f fb =
  forAll (text arbitrary) $ \t1 ->
  forAll (text arbitrary) $ \t2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fTxt) ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  C.test (f t1 t2 fTxt (`compare` iRight)) iLeft
    == fb iLeft iRight

prop_greaterBy = testByFn M.greaterBy (>)
prop_lessBy = testByFn M.lessBy (<)
prop_greaterEqBy = testByFn M.greaterEqBy (>=)
prop_lessEqBy = testByFn M.lessEqBy (<=)

prop_notEqBy =
  forAll (text arbitrary) $ \t1 ->
  forAll (text arbitrary) $ \t2 ->
  forAll (fmap Blind $ function1 coarbitrary (text arbitrary))
    $ \(Blind fTxt) ->
  forAll (fmap Blind $ function2 coarbitrary coarbitrary arbitrary)
    $ \(Blind fBool) ->
  forAll arbitrary $ \iLeft ->
  forAll arbitrary $ \iRight ->
  let _types = (iLeft, iRight) :: (Int, Int) in
  C.test (M.notEqBy t1 t2 fTxt (`fBool` iRight)) iLeft
    == (not $ fBool iLeft iRight)


testParseComparer
  :: Text
  -- ^ Comparer text to parse
  -> (Text -> Int -> C.Pred Int)
  -- ^ The comparer that should actually be returned
  -> Property
testParseComparer x p =
  forAll (text arbitrary) $ \txt ->
  forAll arbitrary $ \rhs ->
  let get = M.compare txt rhs in
  case M.parseComparer x get of
    Nothing -> property False
    Just p' -> forAll arbitrary $ \lhs ->
      C.test (p txt rhs) lhs == C.test p' lhs

prop_textEQ1 = testParseComparer "=" M.equal
prop_textEQ2 = testParseComparer "==" M.equal
prop_textG = testParseComparer ">" M.greater
prop_textL = testParseComparer "<" M.less
prop_textGE = testParseComparer ">=" M.greaterEq
prop_textLE = testParseComparer "<=" M.lessEq
prop_textNE1 = testParseComparer "/=" M.notEq
prop_textNE2 = testParseComparer "!=" M.notEq
