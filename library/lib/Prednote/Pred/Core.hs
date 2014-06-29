{-# LANGUAGE BangPatterns #-}
-- | 'Pred' core functions.  If your needs are simple, "Prednote.Pred"
-- is easier to use.  However, the types and functions in this module
-- give you more control.
module Prednote.Pred.Core where

import System.Console.Rainbow
import Prelude hiding (filter, not)
import qualified Prelude
import Data.Functor.Contravariant (Contravariant(..))
import Data.Tree

type Chunker = Int -> [Chunk]

data Pred a = Pred
  { static :: Tree Chunker
  , eval :: a -> Tree Output
  }

instance Show (Pred a) where
  show _ = "Predicate"

instance Contravariant Pred where
  contramap f (Pred s e) = Pred s (e . f)

data Output = Output
  { result :: Bool
  , short :: Maybe Chunker
  , dynamic :: Chunker
  }

instance Show Output where
  show (Output r _ _) = "output: " ++ show r


predicate
  :: Chunker
  -> (a -> (Bool, Chunker))
  -> Pred a
predicate st f = Pred (Node st [])
  (\a -> let (r, c) = f a in Node (Output r Nothing c) [])


and
  :: Chunker
  -- ^ Static label
  -> Chunker
  -- ^ Short-circuit label
  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label
  -> [Pred a]
  -> Pred a
and st ss dyn ls = Pred st' ev
  where
    st' = Node st . map static $ ls
    ev a = go [] ls
      where
        go soFar [] = Node (Output True Nothing (dyn True a))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = eval x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just ss
              out = Output r shrt (dyn r a)
              cs = reverse (tree:soFar)
          in case xs of
              [] -> Node out cs
              _ | Prelude.not r -> Node out cs
                | otherwise -> go cs xs


or
  :: Chunker
  -- ^ Static label
  -> Chunker
  -- ^ Short-circuit label
  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label
  -> [Pred a]
  -> Pred a
or st ss dyn ls = Pred st' ev
  where
    st' = Node st . map static $ ls
    ev a = go [] ls
      where
        go soFar [] = Node (Output False Nothing (dyn False a))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = eval x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just ss
              out = Output r shrt (dyn r a)
              cs = reverse (tree:soFar)
          in case xs of
              [] -> Node out cs
              _ | r -> Node out cs
                | otherwise -> go cs xs


not
  :: Chunker
  -- ^ Static label
  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label
  -> Pred a
  -> Pred a
not st dyn pd = Pred st' ev
  where
    st' = Node st [static pd]
    ev a = Node nd [c]
      where
        nd = Output res Nothing (dyn res a)
        (res, c) = (Prelude.not r, t)
          where
            t = eval pd a
            r = result . rootLabel $ t

fan
  :: ([Bool] -> (Bool, Maybe Int))

  -> Chunker
  -- ^ Static label

  -> Chunker
  -- ^ Short-circuit label

  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label

  -> (a -> [b])
  -- ^ Fanout function

  -> Pred b
  -> Pred a
fan get st ss dyn fn pd = Pred st' ev
  where
    st' = Node st [static pd]
    ev a = Node nd cs
      where
        nd = Output r shrt (dyn r a)
        bs = fn a
        allcs = map (eval pd) bs
        bools = map (result . rootLabel) allcs
        (r, mayInt) = get bools
        cs = case mayInt of
          Nothing -> allcs
          Just i -> take i allcs
        shrt | cs `shorter` allcs = Just ss
             | otherwise = Nothing

fanand
  :: Chunker
  -- ^ Static label

  -> Chunker
  -- ^ Short-circuit label

  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label

  -> (a -> [b])
  -- ^ Fanout function

  -> Pred b
  -> Pred a
fanand = fan get
  where
    get = go 0
      where
        go !c ls = case ls of
          [] -> (True, Just c)
          x:xs
            | Prelude.not x -> (False, Just (c + 1))
            | otherwise -> go (c + 1) xs

fanor
  :: Chunker
  -- ^ Static label

  -> Chunker
  -- ^ Short-circuit label

  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label

  -> (a -> [b])
  -- ^ Fanout function

  -> Pred b
  -> Pred a
fanor = fan get
  where
    get = go 0
      where
        go !c ls = case ls of
          [] -> (False, Just c)
          x:xs
            | x -> (True, Just (c + 1))
            | otherwise -> go (c + 1) xs

fanAtLeast
  :: Int
  -- ^ Find at least this many

  -> Chunker
  -- ^ Static label

  -> Chunker
  -- ^ Short-circuit label

  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label

  -> (a -> [b])
  -- ^ Fanout function

  -> Pred b
  -> Pred a
fanAtLeast i = fan get
  where
    get = go 0 0
      where
        go !found !c ls
          | found >= i = (True, Just c)
          | otherwise = case ls of
              [] -> (False, Just c)
              x:xs -> go fnd' (c + 1) xs
                where
                  fnd' | x = found + 1
                       | otherwise = found


-- | @shorter x y@ is True if list x is shorter than list y. Lazier
-- than taking the length of each list and comparing the results.
shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

