{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Prednote.Core.Properties where

import Prednote.Core.Instances ()
import Prednote.Core
import Test.QuickCheck.Function
import Prelude

testInt :: Pred Int -> Int -> Bool
testInt = test

prop_predicateIsLazyInFirstArgument (Fun _ f) i
  = r || True
  where
    r = testInt (predicate undefined f) i

prop_predicateIsLazyInAnnotation (Fun _ f) i
  = r || True
  where
    r = testInt (predicate undefined fAnn) i
    fAnn x = Annotated undefined (f x)

prop_wrapIsLazyInChunks (Fun _ f) pd i
  = r || True
  where
    r = testInt (wrap undefined f pd) i
    _types = f :: Int -> Annotated Char

prop_wrapIsLazyInAnnotation (Fun _ f) pd i
  = r || True
  where
    r = testInt (wrap undefined fAnn pd) i
    fAnn c = Annotated undefined (f c)
    _types = f :: Int -> Char

prop_wrapIsLazyInStatic (Fun _ fAnn) (Fun _ fOut) i
  = r || True
  where
    r = testInt (wrap undefined fAnn (Pred undefined fOut)) i
    _types = fAnn :: Int -> Annotated Char

prop_switchIsLazyInChunks (Fun _ fAnn) pb pc i
  = test (switch undefined fAnn pb pc) i || True
  where
    _types = fAnn :: Int -> Annotated (Either Char Bool)


prop_switchIsLazyInAnnotation (Fun _ fAnn) pb pc i
  = test (switch undefined fAnn' pb pc) i || True
  where
    fAnn' x = Annotated undefined (fAnn x)
    _types = fAnn :: Int -> Either Int Char

prop_switchIsLazyInFirstPred (Fun _ fInt) pb i
  = testInt (switch undefined fAnn pb undefined) i || True
  where
    fAnn x = Annotated undefined (Left $ fInt x)
    _types = fInt :: Int -> Char


prop_switchIsLazyInSecondPred (Fun _ fInt) pc i
  = testInt (switch undefined fAnn undefined pc) i || True
  where
    fAnn x = Annotated undefined (Right $ fInt x)
    _types = fInt :: Int -> Char
