module Prednote.Tests.Util where

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

function1
  :: (a -> Gen b -> Gen b)
  -> Gen b
  -> Gen (a -> b)
function1 perturb gen = promote (`perturb` gen)

function2
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> Gen r
  -> Gen (a -> b -> r)
function2 p1 p2 = fmap f . function1 p'
  where
    p' (a, b) = p1 a . p2 b
    f g = \a b -> g (a, b)

function3
  :: (a -> Gen r -> Gen r)
  -> (b -> Gen r -> Gen r)
  -> (c -> Gen r -> Gen r)
  -> Gen r
  -> Gen (a -> b -> c -> r)
function3 p1 p2 p3 = fmap f . function1 p'
  where
    p' (a, b, c) = p1 a . p2 b . p3 c
    f g = \a b c -> g (a, b, c)
