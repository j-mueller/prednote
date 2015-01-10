{-# OPTIONS_GHC -fno-warn-orphans #-}
module Prednote.Core.Instances where

import Rainbow.Types.Instances ()
import Test.QuickCheck
import Control.Monad
import Prednote.Core

instance Arbitrary Static where
  arbitrary = liftM2 Static arbitrary arbitrary

instance CoArbitrary Static where
  coarbitrary (Static cs c) = coarbitrary cs . coarbitrary c

instance Arbitrary Children where
  arbitrary = oneof
    [ return Empty
    , fmap One arbitrary
    , liftM2 Two arbitrary arbitrary
    ]

instance CoArbitrary Children where
  coarbitrary c = case c of
    Empty -> varInt 0
    One s -> varInt 1 . coarbitrary s
    Two s1 s2 -> varInt 2 . coarbitrary s1 . coarbitrary s2

instance CoArbitrary a => Arbitrary (Pred a) where
  arbitrary = liftM2 Pred arbitrary arbitrary

instance Arbitrary a => CoArbitrary (Pred a) where
  coarbitrary (Pred s f) = coarbitrary s . coarbitrary f

instance Arbitrary OutC where
  arbitrary = oneof
    [ fmap Terminal arbitrary
    , liftM2 Hollow arbitrary arbitrary
    , liftM3 Child1 arbitrary arbitrary arbitrary
    , liftM4 Child2 arbitrary arbitrary arbitrary arbitrary
    ]

varInt :: Int -> Gen b -> Gen b
varInt = variant

instance CoArbitrary OutC where
  coarbitrary x = case x of
    Terminal a -> varInt 0 . coarbitrary a
    Hollow a b -> varInt 1 . coarbitrary a . coarbitrary b
    Child1 a b c -> varInt 2 . coarbitrary a . coarbitrary b
      . coarbitrary c
    Child2 a b c d -> varInt 3 . coarbitrary a . coarbitrary b
      . coarbitrary c . coarbitrary d

instance Arbitrary Out where
  arbitrary = liftM2 Out arbitrary arbitrary

instance CoArbitrary Out where
  coarbitrary (Out a b) = coarbitrary a . coarbitrary b

instance Arbitrary ShowKids where
  arbitrary = fmap ShowKids arbitrary

instance CoArbitrary ShowKids where
  coarbitrary (ShowKids a) = coarbitrary a

instance Arbitrary ShowInfo where
  arbitrary = fmap ShowInfo arbitrary

instance CoArbitrary ShowInfo where
  coarbitrary (ShowInfo a) = coarbitrary a

instance Arbitrary a => Arbitrary (Annotated a) where
  arbitrary = liftM2 Annotated arbitrary arbitrary
