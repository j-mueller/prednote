module Prednote.Core.Generators where

import Prednote.Core
import Rainbow.Generators
import Test.QuickCheck
import Control.Applicative
import Prelude.Generators

static :: Gen Static
static = Static <$> listOf chunk <*> children

children :: Gen Children
children = oneof
  [ pure Empty
  , One <$> static
  , Two <$> static <*> static
  ]

out :: Gen Out
out = Out <$> listOf chunk <*> outC

outC :: Gen OutC
outC = oneof
  [ Terminal <$> arbitrary
  , Hollow <$> out
  , Child1 <$> arbitrary <*> visible <*> out
  , Child2 <$> arbitrary <*> visible <*> out <*> out
  ]

visible :: Gen Visible
visible = Visible <$> arbitrary

pred :: CoArbitrary a => Gen (Pred a)
pred = Pred <$> static <*> function1 coarbitrary out

annotated :: Arbitrary a => Gen (Annotated a)
annotated = Annotated <$> listOf chunk <*> arbitrary
