{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Pred.Core.Properties where

import Prednote.Pred.Core.Generators
import qualified Prednote.Pred.Core as C
import qualified Data.Tree as E
import Test.QuickCheck
import Prednote.Tests.Util
import Prelude hiding (pred)

-- | Shown values are True
prop_shown :: Bool
prop_shown = C.unVisible C.shown

-- | Hidden values are False
prop_hidden :: Bool
prop_hidden = not $ C.unVisible C.hidden

-- | Visiblity has the expected result
prop_visibility =
  forAll (fmap Blind $ function1 coarbitrary visible) $ \(Blind f) ->
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  let r = E.rootLabel . ($ i) . C.evaluate . C.visibility f $ p
  in f (C.result r) == C.visible r

-- | predicate has the expected result
prop_predicate =
  forAll (fmap Blind chunker) $ \(Blind ckr) ->
  forAll (fmap Blind $ function3 coarbitrary arbitrary visible chunker)
    $ \(Blind f) ->
  forAll arbitrary $ \i ->
  forAll pred $ \p ->
  let (exR, exV, _) = f i
      (acR, acV) = (C.result o, C.visible o)
        where
          o = E.rootLabel $ C.evaluate p i
  in exR == acR && exV == acV
