module Prednote.Pred.Wrappers where

import qualified Prednote.Pred as P
import Test.QuickCheck

newtype Visible = Visible { unVisible :: P.Visible }
  deriving (Eq, Ord, Show)

instance Arbitrary Visible where
  arbitrary = fmap (Visible . P.Visible) arbitrary
