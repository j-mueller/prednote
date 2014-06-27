module Prednote.Pred.Properties where

import qualified Prednote.Pred as P
import Test.QuickCheck.Function
import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Prednote.Pred.Core.Wrappers
import Prednote.Pred.Wrappers
import Data.Text.Wrappers
import Data.Maybe

tests :: TestTree
tests = testGroup "Prednote.Pred.Properties"
  [ testGroup "reveal"

    [ testProperty "produces visible Pred" $
      \(Pred pd) i ->
      P.visible . snd . P.node $ P.evaluate (P.reveal pd) i
    ]

  , testGroup "hide"

    [ testProperty "produces hidden Pred" $
      \(Pred pd) i ->
      not . P.visible . snd . P.node $ P.evaluate (P.hide pd) i
    ]

  , testGroup "true"

    [ testProperty "returns True" $
      P.result . snd . P.node . P.evaluate P.true . intify

    , testProperty "returns visible Pred" $
      P.visible . snd . P.node . P.evaluate P.true . intify
    ]

  , testGroup "false"

    [ testProperty "returns False" $
      not . P.result . snd . P.node . P.evaluate P.true . intify

    , testProperty "returns visible Pred" $
      P.visible . snd . P.node . P.evaluate P.false . intify
    ]

  , testGroup "predicate"

    [ testProperty "evaluation returns same as function" $
      \(Fun _ f) (Text t) i ->
      let f' inp = (bl, vs, tx)
            where
              (bl, Visible vs, Text tx) = f inp
          (res, P.Visible vis, _) = f' i
          P.Output vis' res' _ = snd . P.node $
            P.evaluate (P.predicate t f') (intify i)
      in res == res' && vis == vis'
    ]

  , testGroup "and"

    [ testProperty "empty list returns True" $
      \(Fun _ fv) i ->
      P.result . snd . P.node
      $ P.evaluate (P.and fv []) (intify i)

    , testProperty "all True returns True" $
      \(Fun _ fv) (Positive c) i ->
      P.result . snd . P.node
      $ P.evaluate (P.and fv (replicate c P.true)) (intify i)

    , testProperty "any False returns False" $
      \(Fun _ fv) nl nr i ->
      let lft = map unPred nl
          rt = map unPred nr
      in not . P.result . snd . P.node
      $ P.evaluate (P.and fv (lft ++ P.false : rt)) (intify i)

    , testProperty "does not short circuit if only False is last item" $
      \(Fun _ fv) (NonNegative nl) i ->
      isNothing . fst . P.node
      $ P.evaluate (P.and fv (replicate nl P.true ++ [P.false]))
                   (intify i)

    , testProperty "does not short circuit on True" $
      \(Fun _ fv) (NonNegative nl) i ->
      isNothing . fst . P.node
      $ P.evaluate (P.and fv (replicate nl P.true)) (intify i)

    , testProperty "short circuits if False is not last item" $
      \(Fun _ fv) nl (NonEmpty nr) i ->
      let lft = map unPred nl
          rt = map unPred nr
          ls = lft ++ P.false : rt
      in isJust . fst . P.node
         $ P.evaluate (P.and fv ls) (intify i)

    , testProperty "short circuit is correct length" $
      \(Fun _ fv) (NonNegative nl) (NonEmpty nr) i ->
      let lft = replicate nl P.true
          rt = map unPred nr
          ls = lft ++ P.false : rt
          len = nl + 1
      in (== len) . length . P.children
         $ P.evaluate (P.and fv ls) (intify i)
    ]
  ]

intify :: Int -> Int
intify = id
