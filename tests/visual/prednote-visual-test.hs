{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prednote
import qualified Prednote as P
import Prednote.Comparisons
import Prednote.Format
import qualified Data.Text as X
import System.Console.Rainbow

singleInt :: Pred Int
singleInt = P.any
  [ equal "integer" 5
  , greaterEq "integer" 20
  , lessEq "integer" 10
  ]

listInt :: Pred [Int]
listInt = P.any
  [ let l = "length is greater than 4"
        dyn ls = (X.pack . show $ ls) <+> l
    in predicate l dyn ((> 4) . length)

  , let l = "is even"
        dyn i = (X.pack . show $ i) <+> l
        p = predicate l dyn even
    in fanAll id p

  , fanAny id (equal "integer" 5)
  ]

intList :: [Int]
intList = [1,12,13,2,3,25,20,5,8,11]

intLists :: [[Int]]
intLists =
  [ [4,2,1,9,20]
  , [2,4,6]
  , [6,7,9,5]
  , [-1,4,11,19,20]
  , [2,11]
  ]

main :: IO ()
main = do
  t <- termFromEnv
  putStrLn $ "intList: " ++ show intList
  putStrLn $ "intLists: " ++ show intLists
  putStrLn "singleInt plan:"
  putChunks t (plan singleInt)
  putStrLn ""
  putStrLn "listInt plan:"
  putChunks t (plan listInt)
  putStrLn ""
  let (as, cks) = filterV singleInt intList
  putStrLn "result from filtering int list:"
  putStrLn $ show as
  putStrLn ""
  putChunks t cks
  putStrLn ""
  let (as', cks') = filterV listInt intLists
  putStrLn "result from filtering int lists:"
  putStrLn ""
  putStrLn $ show as'
  putStrLn ""
  putChunks t cks'
        
