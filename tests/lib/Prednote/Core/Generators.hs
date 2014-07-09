module Prednote.Core.Generators where

import Test.QuickCheck hiding (output)
import qualified Prednote.Core as C
import Prednote.Tests.Util
import System.Console.Rainbow.Types.Generators
import Control.Monad
import Prelude hiding (pred)
import Data.Tree.Generators

chunker :: Gen C.Chunker
chunker = function1 variant (listOf chunk)

visible :: Gen C.Visible
visible = fmap C.Visible arbitrary

output :: Gen C.Output
output = liftM4 C.Output arbitrary visible
  (frequency [(3, fmap Just chunker), (1, return Nothing)])
  chunker

pred :: Gen (C.Pred Int)
pred = liftM2 C.Pred (tree chunker) (function1 variant (tree output))

dynamicLabel :: Gen (Bool -> Int -> C.Chunker)
dynamicLabel = function2 coarbitrary coarbitrary chunker
