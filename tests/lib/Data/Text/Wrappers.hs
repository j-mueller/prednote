module Data.Text.Wrappers where

import qualified Data.Text as X
import Test.QuickCheck

newtype Text = Text { unText :: X.Text }
  deriving (Eq, Ord, Show)

instance Arbitrary Text where
  arbitrary = fmap (Text . X.pack) (listOf arbitrary)
