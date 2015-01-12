module Instances where

import Control.Applicative
import Test.QuickCheck
import Rainbow.Types
import qualified Data.Text as X

newtype ChunkA = ChunkA Chunk
  deriving (Eq, Ord, Show)

newtype TextA = TextA X.Text
  deriving (Eq, Ord, Show)

instance Arbitrary TextA where
  arbitrary = (TextA . X.pack) <$> listOf arbitrary

instance Arbitrary Chunk where
  arbitrary = undefined
