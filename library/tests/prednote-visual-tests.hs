{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prednote
import Prelude hiding (any)

main :: IO ()
main = do
  _ <- verboseTestStdout (any $ equal (5 :: Int)) [0..10]
  _ <- verboseTestStdout (any $ equal (4 :: Int)) [0..3]
  return ()
