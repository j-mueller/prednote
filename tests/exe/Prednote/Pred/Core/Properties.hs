{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Prednote.Pred.Core.Properties where

import Prednote.Pred.Core.Generators
import qualified Prednote.Pred.Core as C
import qualified Data.Tree as E
import Test.QuickCheck
import Prednote.Tests.Util
import Prelude hiding (pred)
import Control.Monad

-- | predicate has the expected result
prop_predicate =
  forAll (fmap Blind chunker) $ \(Blind ckr) ->
  forAll (fmap Blind computer) $ \(Blind f) ->
  forAll arbitrary $ \i ->
  let (exR, exV, _) = f i
      (acR, acV) = (C.result o, C.visible o)
        where
          o = E.rootLabel $ C.evaluate (C.predicate ckr f) i
      _types = i :: Int
  in exR == acR && exV == acV
  where
    computer = function1 coarbitrary
      (liftM3 (,,) arbitrary visible chunker)

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

-- | reveal always creates shown Pred
prop_reveal =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  (== C.shown) . C.visible . E.rootLabel . ($ i)
    . C.evaluate . C.reveal $ p

-- | hide always creates hidden Pred
prop_hide =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  (== C.hidden) . C.visible . E.rootLabel . ($ i)
    . C.evaluate . C.hide $ p

-- | showTrue shows Pred if the result is True
prop_showTrue =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  getResult . E.rootLabel . ($ i)
    . C.evaluate . C.showTrue $ p
  where
    getResult o = C.unVisible (C.visible o) == C.result o

-- | showFalse shows Pred if the result is False

prop_showFalse =
  forAll pred $ \p ->
  forAll arbitrary $ \i ->
  getResult . E.rootLabel . ($ i) . C.evaluate . C.showFalse $ p
  where
    getResult o = not (C.unVisible . C.visible $ o) == C.result o

-- | all is True on empty lists of Pred
prop_allIsTrueOnEmptyPred =
  forAll (fmap Blind chunker) $ \(Blind st) ->
  forAll (fmap Blind chunker) $ \(Blind sh) ->
  forAll (fmap Blind dynamicLabel) $ \(Blind dyn) ->
  forAll arbitrary $ \i ->
  C.result . E.rootLabel . ($ i) . C.evaluate $
    C.all st sh dyn []

