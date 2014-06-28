{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Prednote.Pred.Core.Wrappers where

import Test.QuickCheck
import qualified Prednote.Pred.Core as C
import Control.Monad
import System.Console.Rainbow.Wrappers
import qualified System.Console.Rainbow as R
import Test.SmallCheck.Series
import Control.Arrow

newtype Output = Output { unOutput :: C.Output }
  deriving Show

instance Arbitrary Output where
  arbitrary = fmap Output $ liftM3 C.Output arbitrary arbitrary
    intToChunk

instance Monad m => Serial m Output where
  series = fmap Output $ cons3 f
    where
      f b res rpt = C.Output b res (fmap (map unChunk) rpt)

instance Monad m => CoSerial m Output where
  coseries rs =
    alts3 rs >>- \f3 ->
    return $ \(Output (C.Output b res rpt)) ->
    f3 b res (fmap (map Chunk) rpt)

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

instance Monad m => Serial m Shortable where
  series = fmap Shortable $ cons2 f
    where
      f mayShort out = C.Shortable shrt (unOutput out)
        where
          shrt = case mayShort of
            Nothing -> Nothing
            Just (i, fn) -> Just (i, fmap (map unChunk) fn)

instance Monad m => CoSerial m Shortable where
  coseries rs =
    alts2 rs >>- \f2 ->
    return $ \(Shortable (C.Shortable shrt out)) ->
    f2 (fmap (second (fmap (map Chunk))) shrt) (Output out)

newtype Calc = Calc { unCalc :: C.Calc Int }
  deriving Show

expand :: Int -> [Int]
expand i = everyOther [negate (abs i) .. abs i]
  where
    everyOther ls = case ls of
      [] -> []
      x:[] -> [x]
      x:_:xs -> x : everyOther xs

instance Monad m => Serial m Calc where
  series = fmap Calc $
    cons1 (\pd -> C.Predicate (fmap unOutput pd))
    \/ cons2 (\(Pred pd) f -> C.Single pd (fmap (fmap unOutput) f))
    \/ cons2 (\ls f -> C.Variable (map unPred ls)
                       (fmap (fmap unShortable) f))
    \/ cons3 (\g pd f -> C.Fan g (unPred pd)
                         (fmap (fmap unShortable) f))

instance Monad m => CoSerial m Calc where
  coseries rs =
    alts1 rs >>- \f1 ->
    alts2 rs >>- \f2a ->
    alts2 rs >>- \f2b ->
    alts3 rs >>- \f3 ->
    return $ \(Calc c) -> case c of
      C.Predicate p -> f1 (fmap Output p)
      C.Single p o -> f2a (Pred p) (fmap (fmap Output) o)
      C.Variable ps o -> f2b (map Pred ps) (fmap (fmap Shortable) o)
      C.Fan g pd f -> f3 g (Pred pd) (fmap (fmap Shortable) f)

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

instance Monad m => Serial m Pred where
  series = undefined

instance Monad m => CoSerial m Pred where
  coseries = undefined

instance Arbitrary Pred where
  arbitrary = fmap Pred $
    liftM2 C.Pred intToChunk (fmap unCalc arbitrary)

