{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rainbow.Types.Instances where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Rainbow.Types
import qualified Data.Text as X

instance Arbitrary a => Arbitrary (Last a) where
  arbitrary = Last <$> arbitrary

instance CoArbitrary a => CoArbitrary (Last a) where
  coarbitrary (Last m) = case m of
    Nothing -> varInt 0
    Just x -> varInt 1 . coarbitrary x

instance Arbitrary Enum8 where
  arbitrary = elements [ E0, E1, E2, E3, E4, E5, E6, E7 ]

varInt :: Int -> Gen b -> Gen b
varInt = variant

instance CoArbitrary Enum8 where
  coarbitrary a = case a of
    { E0 -> varInt 0; E1 -> varInt 1; E2 -> varInt 2; E3 -> varInt 3;
      E4 -> varInt 4; E5 -> varInt 5; E6 -> varInt 6; E7 -> varInt 7 }

instance Arbitrary Color8 where
  arbitrary = Color8 <$> arbitrary

instance CoArbitrary Color8 where
  coarbitrary (Color8 a) = coarbitrary a

instance Arbitrary Color256 where
  arbitrary = Color256 <$> arbitrary

instance CoArbitrary Color256 where
  coarbitrary (Color256 a) = coarbitrary a

instance Arbitrary StyleCommon where
  arbitrary = liftM4 StyleCommon arbitrary arbitrary arbitrary
    arbitrary

instance CoArbitrary StyleCommon where
  coarbitrary (StyleCommon a b c d) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d

instance Arbitrary Style8 where
  arbitrary = liftM3 Style8 arbitrary arbitrary arbitrary

instance CoArbitrary Style8 where
  coarbitrary (Style8 a b c) =
    coarbitrary a . coarbitrary b . coarbitrary c

instance Arbitrary Style256 where
  arbitrary = liftM3 Style256 arbitrary arbitrary arbitrary

instance CoArbitrary Style256 where
  coarbitrary (Style256 a b c) =
    coarbitrary a . coarbitrary b . coarbitrary c

instance Arbitrary TextSpec where
  arbitrary = liftM2 TextSpec arbitrary arbitrary

instance CoArbitrary TextSpec where
  coarbitrary (TextSpec x y) = coarbitrary x . coarbitrary y

instance Arbitrary X.Text where
  arbitrary = X.pack <$> listOf arbitrary

instance CoArbitrary X.Text where
  coarbitrary = coarbitrary . X.unpack

instance Arbitrary Chunk where
  arbitrary = liftM2 Chunk arbitrary arbitrary

instance CoArbitrary Chunk where
  coarbitrary (Chunk x y) = coarbitrary x . coarbitrary y
