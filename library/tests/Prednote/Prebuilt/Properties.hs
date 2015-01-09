{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Prebuilt.Properties where

import Rainbow.Types.Instances ()
import Prednote.Prebuilt
import Prednote.Prebuilt.Internal
import Prednote.Prebuilt.Internal.Instances ()
import Test.QuickCheck.Function
import qualified Prelude
import Prelude hiding (not)

testInt :: Pdct Int -> Int -> Bool
testInt = test

prop_trueReturnsTrue = testInt true

prop_falseReturnsFalse = Prelude.not . testInt false

prop_sameReturnsSame b = test same b == b

prop_predicateDoesSameAsFunction ts txt (Fun _ f) i
  = testInt (predicate ts txt f) i == f i

prop_andDoesSameAsPreludeAnd ts1 txt1 (Fun _ f1)
  ts2 txt2 (Fun _ f2) i
  = (testInt (p1 &&& p2) i) == (f1 i && f2 i)
  where
    p1 = predicate ts1 txt1 f1
    p2 = predicate ts2 txt2 f2

prop_orDoesSameAsPreludeOr ts1 txt1
  (Fun _ f1) ts2 txt2 (Fun _ f2) i
  = (testInt (p1 ||| p2) i) == (f1 i || f2 i)
  where
    p1 = predicate ts1 txt1 f1
    p2 = predicate ts2 txt2 f2

prop_andShortCircuitsOnFalse i
  = (testInt (false &&& (Pdct undefined undefined)) i) == False
