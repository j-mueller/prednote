{-# LANGUAGE BangPatterns #-}
-- | 'Pred' core functions.  If your needs are simple, "Prednote.Pred"
-- is easier to use.  However, the types and functions in this module
-- give you more control.
--
-- Exports some names that conflict with Prelude names, so you might
-- want to do something like
--
-- > import qualified Prednote.Pred.Core as P

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
  , evaluate :: a -> Tree Output
  }

instance Contravariant Pred where
  contramap f (Pred s e) = Pred s (e . f)

data Output = Output
  { result :: Bool
  , visible :: Visible
  , short :: Maybe Chunker
  , dynamic :: Chunker
  }

instance Show Output where
  show (Output r v _ _) = "output - result: " ++ show r
    ++ " visible: " ++ (show . unVisible $ v)


predicate
  :: Chunker
  -- ^ Static name

  -> (a -> (Bool, Visible, Chunker))
  -- ^ Compute result

  -> Pred a
predicate st f = Pred (Node st [])
  (\a -> let (r, v, c) = f a in Node (Output r v Nothing c) [])


newtype Visible = Visible { unVisible :: Bool }
  deriving (Eq, Ord, Show)

shown :: Visible
shown = Visible True

hidden :: Visible
hidden = Visible False

visibility :: (Bool -> Visible) -> Pred a -> Pred a
visibility f (Pred s e) = Pred s e'
  where
    e' a = g (e a)
    g (Node n cs) = Node n { visible = f (result n) } cs

reveal :: Pred a -> Pred a
reveal = visibility (const shown)

hide :: Pred a -> Pred a
hide = visibility (const hidden)

showTrue :: Pred a -> Pred a
showTrue = visibility (\b -> if b then shown else hidden)

showFalse :: Pred a -> Pred a
showFalse = visibility (\b -> if Prelude.not b then shown else hidden)


all
  :: Chunker
  -- ^ Static label
  -> Chunker
  -- ^ Short-circuit label
  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label
  -> [Pred a]
  -> Pred a
all st ss dyn ls = Pred st' ev
  where
    st' = Node st . map static $ ls
    ev a = go [] ls
      where
        go soFar [] = Node (Output True shown Nothing (dyn True a))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = evaluate x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just ss
              out = Output r shown shrt (dyn r a)
              cs = reverse (tree:soFar)
          in case xs of
              [] -> Node out cs
              _ | Prelude.not r -> Node out cs
                | otherwise -> go cs xs


any
  :: Chunker
  -- ^ Static label
  -> Chunker
  -- ^ Short-circuit label
  -> (Bool -> a -> Chunker)
  -- ^ Dynamic label
  -> [Pred a]
  -> Pred a
any st ss dyn ls = Pred st' ev
  where
    st' = Node st . map static $ ls
    ev a = go [] ls
      where
        go soFar [] = Node (Output False shown Nothing (dyn False a))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = evaluate x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just ss
              out = Output r shown shrt (dyn r a)
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
        nd = Output res shown Nothing (dyn res a)
        (res, c) = (Prelude.not r, t)
          where
            t = evaluate pd a
            r = result . rootLabel $ t

fan
  :: ([Bool] -> (Bool, Visible, Maybe Int))

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
        allcs = map (evaluate pd) bs
        bools = map (result . rootLabel) allcs
        (r, v, mayInt) = get bools
        cs = case mayInt of
          Nothing -> allcs
          Just i -> take i allcs
        shrt | cs `shorter` allcs = Just ss
             | otherwise = Nothing

fanAll
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
fanAll = fan get
  where
    get = go 0
      where
        go !c ls = case ls of
          [] -> (True, shown, Just c)
          x:xs
            | Prelude.not x -> (False, shown, Just (c + 1))
            | otherwise -> go (c + 1) xs

fanAny
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
fanAny = fan get
  where
    get = go 0
      where
        go !c ls = case ls of
          [] -> (False, shown, Just c)
          x:xs
            | x -> (True, shown, Just (c + 1))
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
          | found >= i = (True, shown, Just c)
          | otherwise = case ls of
              [] -> (False, shown, Just c)
              x:xs -> go fnd' (c + 1) xs
                where
                  fnd' | x = found + 1
                       | otherwise = found

report
  :: Int
  -> Tree Output
  -> [Chunk]
report l (Node n cs)
  | (== hidden) . visible $ n = []
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

instance Show (Pred a) where
  show = X.unpack . X.concat . concat . map text
    . plan 0

test :: Pred a -> a -> Bool
test p = result . rootLabel . evaluate p

testV :: Pred a -> a -> (Bool, [Chunk])
testV p a = (result . rootLabel $ t, report 0 t)
  where
    t = evaluate p a

filter :: Pred a -> [a] -> [a]
filter p = Prelude.filter (test p)

filterV :: Pred a -> [a] -> ([a], [Chunk])
filterV p as = (mapMaybe fltr (zip as rslts), cks)
  where
    fltr (a, r)
      | result . rootLabel $ r = Just a
      | otherwise = Nothing
    rslts = map (evaluate p) as
    cks = concatMap (report 0) rslts

-- | @shorter x y@ is True if list x is shorter than list y. Lazier
-- than taking the length of each list and comparing the results.
shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

