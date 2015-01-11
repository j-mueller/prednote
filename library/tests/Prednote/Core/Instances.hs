{-# OPTIONS_GHC -fno-warn-orphans #-}
module Prednote.Core.Instances where

import Rainbow.Types.Instances ()
import Test.QuickCheck
import Control.Monad
import Prednote.Core

instance Arbitrary Label where
  arbitrary = fmap Label arbitrary

instance CoArbitrary Label where
  coarbitrary (Label cs) = coarbitrary cs

instance Arbitrary Condition where
  arbitrary = fmap Condition arbitrary

instance CoArbitrary Condition where
  coarbitrary (Condition cs) = coarbitrary cs

instance Arbitrary Value where
  arbitrary = fmap Value arbitrary

instance CoArbitrary Value where
  coarbitrary (Value s) = coarbitrary s

instance Arbitrary Passed where
  arbitrary = sized f
    where
      f s | s < 10 = liftM3 PTerminal arbitrary arbitrary arbitrary
          | otherwise = oneof
              [ liftM3 PTerminal arbitrary arbitrary arbitrary
              , liftM2 PAnd nestPass nestPass
              , fmap POr (oneof [ fmap Left nestPass,
                                  fmap Right (liftM2 (,) nestFail nestPass) ])
              , fmap PNot nestFail
              ]
        where
          nestPass = resize (s `mod` 4) arbitrary
          nestFail = resize (s `mod` 4) arbitrary

instance Arbitrary Failed where
  arbitrary = sized f
    where
      f s | s < 10 = liftM3 FTerminal arbitrary arbitrary arbitrary
          | otherwise = oneof
              [ liftM3 FTerminal arbitrary arbitrary arbitrary
              , fmap FAnd (oneof [ fmap Left nestFail
                                 , fmap Right (liftM2 (,) nestPass nestFail) ])
              , liftM2 FOr nestFail nestFail
              , fmap FNot nestPass
              ]
        where
          nestPass = resize (s `mod` 4) arbitrary
          nestFail = resize (s `mod` 4) arbitrary

instance CoArbitrary a => Arbitrary (Pred a) where
  arbitrary = fmap Pred arbitrary

{-

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
-}
