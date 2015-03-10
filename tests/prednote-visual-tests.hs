{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Prednote
import Prelude hiding (any, all, maybe)

main :: IO ()
main = do
  verboseTestStdout (all $ lessEq (5 :: Int)) [0..10]
  verboseTestStdout (any $ equal (4 :: Int)) [0..3]
  verboseTestStdout (any $ equal (10 :: Int)) []
  verboseTestStdout (all $ maybe True (lessEq (5 :: Int)))
    [Nothing, Just 1, Just 2, Nothing, Just 3, Just 4, Just 5]
  return ()
