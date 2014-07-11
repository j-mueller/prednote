module Data.Tree.Generators where

import Data.Tree
import Test.QuickCheck
import Control.Monad

tree :: Gen a -> Gen (Tree a)
tree g = sized h
  where
    h s = liftM2 Node g cs
      where
        cs | s <= 0 = return []
           | otherwise = resize (s `div` 8) (listOf (tree g))
