module Prednote.Pred.Properties where

import qualified Prednote.Pred as P
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Prednote.Pred.Properties"
  [ testProperty "true returns True" $
    P.result . snd . P.node . P.evaluate P.true . intify

  , testProperty "false returns False" $
    not . P.result . snd . P.node . P.evaluate P.true . intify
  ]

intify :: Int -> Int
intify = id
