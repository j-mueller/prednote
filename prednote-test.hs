module Main where

import Control.Applicative
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Test.QuickCheck (Arbitrary, Gen, arbitrary)
import qualified Data.Prednote.Pdct as P
import qualified Data.Text as X

main :: IO ()
main = undefined

instance Arbitrary X.Text where
  arbitrary = fmap X.pack (Q.listOf (Q.choose ('!', '~')))

instance Q.CoArbitrary a => Arbitrary (P.Pdct a) where
  arbitrary = P.Pdct <$> arbitrary <*> arbitrary

instance Q.CoArbitrary a => Arbitrary (P.Node a) where
  arbitrary = Q.sized tree
    where
      tree 0 = fmap P.Operand arbitrary
      tree n = Q.oneof
        [ fmap P.Operand arbitrary
        , fmap P.And (Q.listOf subtree)
        , fmap P.Or (Q.listOf subtree)
        , fmap P.Not subtree
        , fmap P.NeverFalse subtree
        , fmap P.NeverTrue subtree ]
        where
          subtree = P.Pdct <$> arbitrary <*> tree (n `div` 2)
