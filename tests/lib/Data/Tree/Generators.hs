module Data.Tree.Generators where

import Data.Tree
import Test.QuickCheck
import Control.Monad

tree :: Gen a -> Gen (Tree a)
tree g = liftM2 Node g (listOf (tree g))
