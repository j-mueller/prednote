{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import qualified Test.QuickCheck as Q
import Test.QuickCheck.All (quickCheckAll)
import Test.QuickCheck.Function
import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import qualified Prednote.Predbox as P
import Prednote.Predbox ((&&&), (|||))
import qualified Data.Text as X
import qualified System.Exit as Exit

instance Arbitrary X.Text where
  arbitrary = fmap X.pack (Q.listOf (Q.choose ('!', '~')))

instance Q.CoArbitrary a => Arbitrary (P.Predbox a) where
  arbitrary = P.Predbox <$> arbitrary <*> arbitrary <*> arbitrary

instance Q.CoArbitrary a => Arbitrary (P.Node a) where
  arbitrary = Q.sized tree
    where
      tree 0 = fmap P.Predicate arbitrary
      tree n = Q.oneof
        [ fmap P.And (Q.listOf subtree)
        , fmap P.Or (Q.listOf subtree)
        , fmap P.Not subtree ]
        where
          subtree = P.Predbox <$> arbitrary <*> arbitrary
                    <*> tree (n `div` 2)

-- | And is commutative
prop_andCommutative :: a -> P.Predbox a -> P.Predbox a -> Bool
prop_andCommutative a p1 p2 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate (p1 &&& p2) a
    r2 = P.evaluate (p2 &&& p1) a

-- | And is associative
prop_andAssociative :: a -> P.Predbox a -> P.Predbox a -> P.Predbox a -> Bool
prop_andAssociative a p1 p2 p3 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate (p1 &&& (p2 &&& p3)) a
    r2 = P.evaluate ((p1 &&& p2) &&& p3) a
    
-- | Or is commutative
prop_orCommutative :: a -> P.Predbox a -> P.Predbox a -> Bool
prop_orCommutative a p1 p2 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate (p1 ||| p2) a
    r2 = P.evaluate (p2 ||| p1) a

-- | Or is associative
prop_orAssociative :: a -> P.Predbox a -> P.Predbox a -> P.Predbox a -> Bool
prop_orAssociative a p1 p2 p3 = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate (p1 ||| (p2 ||| p3)) a
    r2 = P.evaluate ((p1 ||| p2) ||| p3) a

-- | Anything or'd with True is True
prop_orWithTrue :: a -> P.Predbox a -> Bool
prop_orWithTrue a p1 = P.rBool r1
  where
    r1 = P.evaluate (p1 ||| P.always) a

-- | Anything and'ed with False is False
prop_andWithFalse :: a -> P.Predbox a -> Bool
prop_andWithFalse a p1 = not $ P.rBool r1
  where
    r1 = P.evaluate (p1 &&& P.never) a

-- | And Distributivity
prop_andDistributivity :: a -> P.Predbox a -> P.Predbox a -> P.Predbox a -> Bool
prop_andDistributivity x a b c = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate (a &&& (b ||| c)) x
    r2 = P.evaluate ((a &&& b) ||| (a &&& c)) x

prop_orDistributivity :: a -> P.Predbox a -> P.Predbox a -> P.Predbox a -> Bool
prop_orDistributivity x a b c = P.rBool r1 == P.rBool r2
  where
    r1 = P.evaluate (a ||| (b &&& c)) x
    r2 = P.evaluate ((a ||| b) &&& (a ||| c)) x

runTests = $quickCheckAll

main :: IO ()
main = do
  b <- runTests
  if b then Exit.exitSuccess else Exit.exitFailure
