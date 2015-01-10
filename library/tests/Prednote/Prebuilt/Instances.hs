{-# OPTIONS_GHC -fno-warn-orphans #-}
module Prednote.Prebuilt.Instances where

import Test.QuickCheck
import Prednote.Prebuilt
import Test.QuickCheck.Function
import Prednote.Core.Instances ()
import qualified Data.Text as X
import Control.Monad

instance Arbitrary Typedesc where
  arbitrary = sized $ \s ->
    let text = do
          i <- choose (1, 3)
          cs <- vectorOf i arbitrary
          return $ X.pack cs
        nest = resize (s `mod` 4) arbitrary
    in frequency
    [ (10, fmap List nest)
    , (5, return Unit)
    , (10, liftM2 Tuple2 nest nest)
    , (5, liftM3 Tuple3 nest nest nest)
    , (5, liftM4 Tuple4 nest nest nest nest)
    , (5, liftM5 Tuple5 nest nest nest nest nest)
    , (60, liftM2 User text (listOf nest))
    ]

instance (CoArbitrary a, Show a)
  => Arbitrary (Typeshow a) where
  arbitrary = liftM2 Typeshow arbitrary (return (X.pack . show))

instance Arbitrary a => CoArbitrary (Typeshow a) where
  coarbitrary (Typeshow d f) = coarbitrary d . coarbitrary f

varInt :: Int -> Gen b -> Gen b
varInt = variant

instance CoArbitrary Typedesc where
  coarbitrary x = case x of
    List d -> varInt 0 . coarbitrary d
    Unit -> varInt 1
    Tuple2 a b -> varInt 2 . coarbitrary a . coarbitrary b
    Tuple3 a b c -> varInt 3 . coarbitrary a . coarbitrary b
      . coarbitrary c
    Tuple4 a b c d -> varInt 4 . coarbitrary a . coarbitrary b
      . coarbitrary c . coarbitrary d
    Tuple5 a b c d e -> varInt 5 . coarbitrary a . coarbitrary b
      . coarbitrary c . coarbitrary d . coarbitrary e
    User t ts -> varInt 6 . coarbitrary (X.unpack t)
      . coarbitrary ts

instance (CoArbitrary a, Show a)
  => Arbitrary (Pdct a) where
  arbitrary = liftM2 Pdct arbitrary arbitrary


instance Arbitrary a => CoArbitrary (Pdct a) where
  coarbitrary (Pdct p t) = coarbitrary p . coarbitrary t

data PdctParts a = PdctParts
  { ppTypeshow :: Typeshow a
  , ppText :: X.Text
  , ppFunction :: Fun a Bool
  , ppPdct :: Pdct a
  } deriving Show

instance (CoArbitrary a, Arbitrary a, Show a, Function a)
  => Arbitrary (PdctParts a) where
  arbitrary = do
    ts <- arbitrary
    txt <- arbitrary
    f@(Fun _ fn) <- arbitrary
    return $ PdctParts ts txt f (predicate ts txt fn)
