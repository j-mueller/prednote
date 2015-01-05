module Prednote.Prebuilt.Generators where

import Test.QuickCheck
import qualified Prednote.Prebuilt as P
import Control.Applicative
import qualified Data.Text as X

text :: Gen X.Text
text = fmap X.pack $ (:) <$> arbitrary <*> listOf arbitrary

typedesc :: Gen P.Typedesc
typedesc = sized $ \s ->
  let nest = frequency
                   [ (50, P.User <$> text
                            <*> resize (s `div` 2) (listOf nest2))
                   , (20, P.List <$> nest2)
                   , (10, pure P.Unit)
                   , (5, P.Tuple2 <$> nest2 <*> nest2)
                   , (5, P.Tuple3 <$> nest2 <*> nest2 <*> nest2)
                   , (5, P.Tuple4 <$> nest2 <*> nest2 <*> nest2 <*> nest2)
                   , (5, P.Tuple5 <$> nest2 <*> nest2 <*> nest2 <*> nest2
                              <*> nest2)
                   ]
      nest2 = P.User <$> text <*> pure []
      
  in frequency
       [ (20, P.List <$> nest)
       , (10, pure P.Unit)
       , (5, P.Tuple2 <$> nest <*> nest)
       , (5, P.Tuple3 <$> nest <*> nest <*> nest)
       , (5, P.Tuple4 <$> nest <*> nest <*> nest <*> nest)
       , (5, P.Tuple5 <$> nest <*> nest <*> nest <*> nest <*> nest)
       , (50, P.User <$> text <*> listOf nest)
       ]
