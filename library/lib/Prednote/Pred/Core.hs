{-# LANGUAGE BangPatterns #-}
-- | 'Pred' core functions.  If your needs are simple, "Prednote.Pred"
-- is easier to use.  However, the types and functions in this module
-- give you more control.
--
-- Exports some names that conflict with Prelude names, such as 'and',
-- 'or', 'not', and 'filter'; keep this in mind when you
-- @import@ this module.

module Prednote.Pred.Core where

import System.Console.Rainbow
import Prelude hiding (filter, not)
import qualified Prelude
import Data.Functor.Contravariant (Contravariant(..))
import Data.Tree
import qualified Data.Text as X
import Data.Maybe

type Chunker = Int -> [Chunk]

data Pred a = Pred
  { static :: Tree Chunker
  , eval :: a -> Tree Output
  }

instance Contravariant Pred where
  contramap f (Pred s e) = Pred s (e . f)

data Output = Output
  { result :: Bool
  , visible :: Bool
  , short :: Maybe Chunker
  , dynamic :: Chunker
  }

instance Show Output where
  show (Output r v _ _) = "output - result: " ++ show r
    ++ " visible: " ++ show v


predicate
  :: Chunker
  -- ^ Static name

  -> (a -> (Bool, Bool, Chunker))
  -- ^ Compute result

  -> Pred a
predicate st f = Pred (Node st [])
  (\a -> let (r, v, c) = f a in Node (Output r v Nothing c) [])


visibility :: (Bool -> Bool) -> Pred a -> Pred a
visibility f (Pred s e) = Pred s e'
  where
    e' a = g (e a)
    g (Node n cs) = Node n { visible = f (result n) } cs

reveal :: Pred a -> Pred a
reveal = visibility (const True)

hide :: Pred a -> Pred a
hide = visibility (const False)

showTrue :: Pred a -> Pred a
showTrue = visibility id

showFalse :: Pred a -> Pred a
showFalse = visibility Prelude.not


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
        go soFar [] = Node (Output True True Nothing (dyn True a))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = eval x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just ss
              out = Output r True shrt (dyn r a)
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
        go soFar [] = Node (Output False True Nothing (dyn False a))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = eval x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just ss
              out = Output r True shrt (dyn r a)
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
        nd = Output res True Nothing (dyn res a)
        (res, c) = (Prelude.not r, t)
          where
            t = eval pd a
            r = result . rootLabel $ t

fan
  :: ([Bool] -> (Bool, Bool, Maybe Int))

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
        nd = Output r v shrt (dyn r a)
        bs = fn a
        allcs = map (eval pd) bs
        bools = map (result . rootLabel) allcs
        (r, v, mayInt) = get bools
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
          [] -> (True, True, Just c)
          x:xs
            | Prelude.not x -> (False, True, Just (c + 1))
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
          [] -> (False, True, Just c)
          x:xs
            | x -> (True, True, Just (c + 1))
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
          | found >= i = (True, True, Just c)
          | otherwise = case ls of
              [] -> (False, True, Just c)
              x:xs -> go fnd' (c + 1) xs
                where
                  fnd' | x = found + 1
                       | otherwise = found

report
  :: Int
  -> Tree Output
  -> [Chunk]
report l (Node n cs)
  | Prelude.not . visible $ n = []
  | otherwise = this ++ concatMap (report (l + 1)) cs ++ shrt
  where
    this = dynamic n l
    shrt = maybe [] ($ l) . short $ n

plan
  :: Int
  -> Pred a
  -> [Chunk]
plan lvl pd = go lvl (static pd)
  where
    go l (Node n cs) = this ++ concatMap (go (l + 1)) cs
      where
        this = n l

evaluate :: Pred a -> a -> Tree Output
evaluate = eval

instance Show (Pred a) where
  show = X.unpack . X.concat . concat . map text
    . plan 0

test :: Pred a -> a -> Bool
test p = result . rootLabel . eval p

testV :: Pred a -> a -> (Bool, [Chunk])
testV p a = (result . rootLabel $ t, report 0 t)
  where
    t = eval p a

filter :: Pred a -> [a] -> [a]
filter p = Prelude.filter (test p)

filterV :: Pred a -> [a] -> ([a], [Chunk])
filterV p as = (mapMaybe fltr (zip as rslts), cks)
  where
    fltr (a, r)
      | result . rootLabel $ r = Just a
      | otherwise = Nothing
    rslts = map (eval p) as
    cks = concatMap (report 0) rslts

-- | @shorter x y@ is True if list x is shorter than list y. Lazier
-- than taking the length of each list and comparing the results.
shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

