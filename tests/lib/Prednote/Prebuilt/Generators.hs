module Prednote.Prebuilt.Generators where

import Test.QuickCheck
import qualified Prednote.Prebuilt as P
import Control.Applicative
import Data.Text.Generators

typedesc :: Gen P.Typedesc
typedesc = sized $ \s ->
  let nest = resize (s `div` 2) typedesc
  in oneof
       [ P.List <$> nest
       , pure P.Unit
       , P.Tuple2 <$> nest <*> nest
       , P.Tuple3 <$> nest <*> nest <*> nest
       , P.Tuple4 <$> nest <*> nest <*> nest <*> nest
       , P.Tuple5 <$> nest <*> nest <*> nest <*> nest <*> nest
       , P.User <$> text arbitrary <*> listOf nest
       ]
