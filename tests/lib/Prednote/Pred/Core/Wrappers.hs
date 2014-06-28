{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Prednote.Pred.Core.Wrappers where

import Test.QuickCheck
import qualified Prednote.Pred.Core as C
import Control.Monad
import System.Console.Rainbow.Wrappers
import qualified System.Console.Rainbow as R
import Test.SmallCheck.Series

newtype Output = Output { unOutput :: C.Output }
  deriving Show

instance Arbitrary Output where
  arbitrary = fmap Output $ liftM3 C.Output arbitrary arbitrary
    intToChunk

intToChunk :: Gen (Int -> [R.Chunk])
intToChunk = fmap (fmap (map unChunk)) arbitrary

newtype Shortable = Shortable { unShortable :: C.Shortable }
  deriving Show

instance Arbitrary Shortable where
  arbitrary = fmap Shortable $
    liftM2 C.Shortable
      (frequency [(1, return Nothing),
        (4, fmap Just $ liftM2 (,) arbitrary intToChunk)])
      (fmap unOutput arbitrary)

newtype Calc = Calc { unCalc :: C.Calc Int }
  deriving Show

instance Arbitrary Calc where
  arbitrary = sized $ \s -> fmap Calc $ oneof
    [ fmap C.Predicate (fmap (fmap unOutput) arbitrary)

    , liftM2 C.Single (resize (s `div` 2) (fmap unPred arbitrary))
        (fmap (fmap (fmap unOutput)) arbitrary)

    , liftM2 C.Variable (resize (s `div` 2) (fmap (map unPred) arbitrary))
        (fmap (fmap (fmap unShortable)) arbitrary)

    , liftM3 C.Fan (return shrink)
        (resize (s `div` 2) (fmap unPred arbitrary))
        (fmap (fmap (fmap unShortable)) arbitrary)
    ]

newtype Pred = Pred { unPred :: C.Pred Int }
  deriving Show

instance Arbitrary Pred where
  arbitrary = fmap Pred $
    liftM2 C.Pred intToChunk (fmap unCalc arbitrary)

