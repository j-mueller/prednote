{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Prebuilt.Properties where

import Prednote.Prebuilt

testInt :: Pdct Int -> Int -> Bool
testInt = test

prop_trueReturnsTrue i = testInt true i
