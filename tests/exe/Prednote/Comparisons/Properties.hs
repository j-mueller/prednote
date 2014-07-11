{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Comparisons.Properties where

import Data.Text.Generators
import Test.QuickCheck
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
