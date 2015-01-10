module Main where

import Prednote
import Prelude hiding (any)

main :: IO ()
main = do
  _ <- ioTest (any $ equal (User "Int" []) (5 :: Int)) [0..10]
  return ()
