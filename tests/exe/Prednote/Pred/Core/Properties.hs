module Prednote.Pred.Core.Properties where

import Prednote.Pred.Core.Generators
import qualified Prednote.Pred.Core as C

-- | Shown values are True
prop_shown :: Bool
prop_shown = C.unVisible C.shown

-- | Hidden values are False
prop_hidden :: Bool
prop_hidden = not $ C.unVisible C.hidden


