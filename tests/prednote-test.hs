module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import qualified Prednote.Pred.Properties

tests :: TestTree
tests = testGroup "Prednote tests"
  [ Prednote.Pred.Properties.tests
  ]

main :: IO ()
main = defaultMain tests
