{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Test.Tasty
import qualified Prednote.Core.Properties

main :: IO ()
main = defaultMain $ testGroup "all tests"
  [ Prednote.Core.Properties.tests
  ]
