{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import qualified Test.QuickCheck as Q
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Function
import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import qualified Data.Prednote.Pdct as P
import Data.Prednote.Pdct ((&&&), (|||))
import qualified Data.Text as X
import qualified System.Exit as Exit

instance Arbitrary X.Text where
  arbitrary = fmap X.pack (Q.listOf (Q.choose ('!', '~')))

instance Q.CoArbitrary a => Arbitrary (P.Pdct a) where
  arbitrary = P.Pdct <$> arbitrary <*> arbitrary <*> arbitrary

instance Q.CoArbitrary a => Arbitrary (P.Node a) where
  arbitrary = Q.sized tree
    where
      tree 0 = fmap P.Operand arbitrary
      tree n = Q.oneof
        [ fmap P.And (Q.listOf subtree)
        , fmap P.Or (Q.listOf subtree)
        , fmap P.Not subtree ]
        where
          subtree = P.Pdct <$> arbitrary <*> arbitrary
                    <*> tree (n `div` 2)

-- | And is commutative
prop_andCommutative :: a -> P.Pdct a -> P.Pdct a -> Bool
prop_andCommutative a p1 p2 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate a (p1 &&& p2)
    r2 = P.evaluate a (p2 &&& p1)

-- | And is associative
prop_andAssociative :: a -> P.Pdct a -> P.Pdct a -> P.Pdct a -> Bool
prop_andAssociative a p1 p2 p3 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate a (p1 &&& (p2 &&& p3))
    r2 = P.evaluate a ((p1 &&& p2) &&& p3)
    
-- | Or is commutative
prop_orCommutative :: a -> P.Pdct a -> P.Pdct a -> Bool
prop_orCommutative a p1 p2 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate a (p1 ||| p2)
    r2 = P.evaluate a (p2 ||| p1)

-- | Or is associative
prop_orAssociative :: a -> P.Pdct a -> P.Pdct a -> P.Pdct a -> Bool
prop_orAssociative a p1 p2 p3 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate a (p1 ||| (p2 ||| p3))
    r2 = P.evaluate a ((p1 ||| p2) ||| p3)

-- | Anything or'd with True is True
prop_orWithTrue :: a -> P.Pdct a -> Bool
prop_orWithTrue a p1 = P.rBool r1
  where
    r1 = P.evaluate a (p1 ||| P.always)

-- | Anything and'ed with False is False
prop_andWithFalse :: a -> P.Pdct a -> Bool
prop_andWithFalse a p1 = not $ P.rBool r1
  where
    r1 = P.evaluate a (p1 &&& P.never)

-- | And Distributitivy
prop_andDistributivity :: a -> P.Pdct a -> P.Pdct a -> P.Pdct a -> Bool
prop_andDistributivity x a b c = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate x $ a &&& (b ||| c)
    r2 = P.evaluate x $ (a &&& b) ||| (a &&& c)

prop_orDistributivity :: a -> P.Pdct a -> P.Pdct a -> P.Pdct a -> Bool
prop_orDistributivity x a b c = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate x $ a ||| (b &&& c)
    r2 = P.evaluate x $ (a ||| b) &&& (a ||| c)

runTests = $quickCheckAll

main :: IO ()
main = do
  b <- runTests
  if b then Exit.exitSuccess else Exit.exitFailure
