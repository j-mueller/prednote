{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Prednote.Core.Instances where

import Rainbow.Instances ()
import Test.QuickCheck hiding (Result)
import Control.Monad
import Prednote.Core

instance (CoArbitrary a, Show a) => Arbitrary (Pred a) where
  arbitrary = fmap predicate arbitrary

instance Arbitrary Condition where
  arbitrary = fmap Condition arbitrary

instance CoArbitrary Condition where
  coarbitrary (Condition c) = coarbitrary c

instance Arbitrary Value where
  arbitrary = fmap Value arbitrary

instance CoArbitrary Value where
  coarbitrary (Value x) = coarbitrary x

instance Arbitrary Label where
  arbitrary = fmap Label arbitrary

instance CoArbitrary Label where
  coarbitrary (Label x) = coarbitrary x

instance Arbitrary a => Arbitrary (Labeled a) where
  arbitrary = liftM2 Labeled arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Labeled a) where
  coarbitrary (Labeled a b) = coarbitrary a . coarbitrary b

instance Arbitrary Passed where
  arbitrary = sized f
    where
      f s | s < 10 = liftM2 PTerminal arbitrary arbitrary
          | otherwise = oneof
              [ liftM2 PTerminal arbitrary arbitrary
              , liftM2 PAnd nestPass nestPass
              , fmap POr
                (oneof [ fmap Left nestPass,
                         fmap Right (liftM2 (,) nestFail nestPass)
                       ])
              , fmap PNot nestFail
              ]
        where
          nestPass = resize (s `div` 4) arbitrary
          nestFail = resize (s `div` 4) arbitrary  
      
instance Arbitrary Failed where
  arbitrary = sized f
    where
      f s | s < 10 = liftM2 FTerminal arbitrary arbitrary
          | otherwise = oneof
              [ liftM2 FTerminal arbitrary arbitrary
              , fmap FAnd
                (oneof [ fmap Left nestFail
                       , fmap Right (liftM2 (,) nestPass nestFail)
                       ])
              , liftM2 FOr nestFail nestFail
              , fmap FNot nestPass
              ]
        where
          nestPass = resize (s `div` 4) arbitrary
          nestFail = resize (s `div` 4) arbitrary

varInt :: Int -> Gen a -> Gen a
varInt = variant

instance CoArbitrary Passed where
  coarbitrary pass = case pass of
    PTerminal v c -> varInt 0 . coarbitrary v . coarbitrary c
    PAnd y1 y2 -> varInt 1 . coarbitrary y1 . coarbitrary y2
    POr e -> varInt 2 . coarbitrary e
    PNot n -> varInt 3 . coarbitrary n

instance CoArbitrary Failed where
  coarbitrary fll = case fll of
    FTerminal v c -> varInt 0 . coarbitrary v . coarbitrary c
    FAnd e -> varInt 1 . coarbitrary e
    FOr x y -> varInt 2 . coarbitrary x . coarbitrary y
    FNot x -> varInt 3 . coarbitrary x

instance Arbitrary Result where
  arbitrary = fmap Result arbitrary

instance CoArbitrary Result where
  coarbitrary (Result x) = coarbitrary x

