module Main where

import Decrees
import Quickpull
import Test.QuickCheck

args :: Args
args = stdArgs { maxSize = 50 }

main :: IO ()
main = defaultMainWith args decrees
