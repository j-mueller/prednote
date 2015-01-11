{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Prednote.Core.Properties where

import Prednote.Core.Instances ()
import Prednote.Core
import Test.QuickCheck.Function
import Prelude hiding (not, any, all)
import qualified Prelude

testInt :: Pred Int -> Int -> Bool
testInt = test

prop_predicateIsLazyInArguments (Fun _ f) i
  = testInt (predicate undefined undefined f) i || True

prop_predicateIsSameAsOriginal (Fun _ f) lbl cond i
  = testInt (predicate lbl cond f) i == f i

prop_andIsLazyInSecondArgument i
  = testInt (false &&& undefined) i || True

prop_orIsLazyInSecondArgument i
  = testInt (true ||| undefined) i || True

prop_andIsLikePreludeAnd (Fun _ f1) (Fun _ f2) i
  = testInt (p1 &&& p2) i == (f1 i && f2 i)
  where
    p1 = predicate undefined undefined f1
    p2 = predicate undefined undefined f2

prop_orIsLikePreludeOr (Fun _ f1) (Fun _ f2) i
  = testInt (p1 ||| p2) i == (f1 i || f2 i)
  where
    p1 = predicate undefined undefined f1
    p2 = predicate undefined undefined f2

prop_notIsLikePreludeNot (Fun _ f1) i
  = testInt (not p1) i == Prelude.not (f1 i)
  where
    p1 = predicate undefined undefined f1

prop_switchIsLazyInFirstArgument pb i
  = test (switch undefined pb) (Right i) || True
  where
    _types = pb :: Pred Int
    
prop_switchIsLazyInSecondArgument pa i
  = test (switch pa undefined) (Left i) || True
  where
    _types = pa :: Pred Int

prop_switch (Fun _ fa) (Fun _ fb) ei
  = test (switch pa pb) ei == expected
  where
    _types = ei :: Either Int Char
    expected = case ei of
      Left i -> fa i
      Right c -> fb c
    pa = predicate undefined undefined fa
    pb = predicate undefined undefined fb
    
prop_true = testInt true

prop_false = Prelude.not . testInt false

prop_same b = test same b == b

prop_any (Fun _ f) ls
  = test (any pa) ls == Prelude.any f ls
  where
    pa = predicate undefined undefined f
    _types = ls :: [Int]
    
prop_all (Fun _ f) ls
  = test (all pa) ls == Prelude.all f ls
  where
    pa = predicate undefined undefined f
    _types = ls :: [Int]
    
-- prop_predicateIsLazyInFirstArgument (Fun _ f) i
--   = r || True
--   where
--     r = testInt (predicate undefined f) i

-- prop_predicateIsLazyInAnnotation (Fun _ f) i
--   = r || True
--   where
--     r = testInt (predicate undefined fAnn) i
--     fAnn x = Annotated undefined (f x)

-- prop_wrapIsLazyInChunks (Fun _ f) pd i
--   = r || True
--   where
--     r = testInt (wrap undefined f pd) i
--     _types = f :: Int -> Annotated Char

-- prop_wrapIsLazyInAnnotation (Fun _ f) pd i
--   = r || True
--   where
--     r = testInt (wrap undefined fAnn pd) i
--     fAnn c = Annotated undefined (f c)
--     _types = f :: Int -> Char

-- prop_wrapIsLazyInStatic (Fun _ fAnn) (Fun _ fOut) i
--   = r || True
--   where
--     r = testInt (wrap undefined fAnn (Pred undefined fOut)) i
--     _types = fAnn :: Int -> Annotated Char

-- prop_switchIsLazyInChunks (Fun _ fAnn) pb pc i
--   = test (switch undefined fAnn pb pc) i || True
--   where
--     _types = fAnn :: Int -> Annotated (Either Char Bool)


-- prop_switchIsLazyInAnnotation (Fun _ fAnn) pb pc i
--   = test (switch undefined fAnn' pb pc) i || True
--   where
--     fAnn' x = Annotated undefined (fAnn x)
--     _types = fAnn :: Int -> Either Int Char

-- prop_switchIsLazyInFirstPred (Fun _ fInt) pb i
--   = testInt (switch undefined fAnn pb undefined) i || True
--   where
--     fAnn x = Annotated undefined (Left $ fInt x)
--     _types = fInt :: Int -> Char


-- prop_switchIsLazyInSecondPred (Fun _ fInt) pc i
--   = testInt (switch undefined fAnn undefined pc) i || True
--   where
--     fAnn x = Annotated undefined (Right $ fInt x)
--     _types = fInt :: Int -> Char
