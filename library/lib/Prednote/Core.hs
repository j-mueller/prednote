{-# LANGUAGE BangPatterns #-}
-- | 'Pred' core functions.  If your needs are simple, "Prednote.Pred"
-- is easier to use.  However, the types and functions in this module
-- give you more control.
--
-- Exports some names that conflict with Prelude names, so you might
-- want to do something like
--
-- > import qualified Prednote.Pred.Core as P

module Prednote.Core where

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

newtype Visible = Visible { unVisible :: Bool }
  deriving (Eq, Ord, Show)

shown :: Visible
shown = Visible True

hidden :: Visible
hidden = Visible False

all :: [Pred a] -> Pred a
all ls = Pred st' ev
  where
    st' = Node (const []) . map static $ ls
    ev a = go [] ls
      where
        go soFar [] = Node (Output True shown Nothing (const []))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = evaluate x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just (const [])
              out = Output r shown shrt (const [])
              cs = reverse (tree:soFar)
          in case xs of
              [] -> Node out cs
              _ | Prelude.not r -> Node out cs
                | otherwise -> go cs xs


any :: [Pred a] -> Pred a
any ls = Pred st' ev
  where
    st' = Node (const []) . map static $ ls
    ev a = go [] ls
      where
        go soFar [] = Node (Output False shown Nothing (const []))
          (reverse soFar)
        go soFar (x:xs) =
          let tree = evaluate x a
              r = result . rootLabel $ tree
              shrt = case xs of
                [] -> Nothing
                _ -> Just (const [])
              out = Output r shown shrt (const [])
              cs = reverse (tree:soFar)
          in case xs of
              [] -> Node out cs
              _ | r -> Node out cs
                | otherwise -> go cs xs


not :: Pred a -> Pred a
not pd = Pred st' ev
  where
    st' = Node (const []) [static pd]
    ev a = Node nd [c]
      where
        nd = Output res shown Nothing (const [])
        (res, c) = (Prelude.not r, t)
          where
            t = evaluate pd a
            r = result . rootLabel $ t

fan
  :: ([Bool] -> (Bool, Visible, Maybe Int))

  -> (a -> [b])
  -- ^ Fanout function

  -> Pred b
  -> Pred a
fan get fn pd = Pred st' ev
  where
    st' = Node (const []) [static pd]
    ev a = Node nd cs
      where
        nd = Output r v shrt (const [])
        bs = fn a
        allcs = map (evaluate pd) bs
        bools = map (result . rootLabel) allcs
        (r, v, mayInt) = get bools
        cs = case mayInt of
          Nothing -> allcs
          Just i -> take i allcs
        shrt | cs `shorter` allcs = Just (const [])
             | otherwise = Nothing

fanAll
  :: (a -> [b])
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
  :: (a -> [b])
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

