{-# LANGUAGE BangPatterns #-}
-- | 'Pred' core functions.  If your needs are simple, "Prednote.Prebuilt"
-- is easier to use.  However, the types and functions in this module
-- give you more control.
--
-- Each function in this module that returns a 'Pred' returns one with
-- the following characteristics:
--
-- * No 'static' name
--
-- Upon evaluation:
--
-- * 'visible' is always 'shown'
--
-- * 'short' is either 'Nothing' or @'Just' ('const' [])@
--
-- * 'dynamic' is always @'const' []@
--
-- Thus, the 'Pred' created by this module are rather bare-bones, but
-- you can modify them as you see fit; "Prednote.Prebuilt" already
-- does this for you.
--
-- This module exports some names that conflict with Prelude names, so
-- you might want to do something like
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

-- | Indicates how to display text.  This function is applied to an
-- 'Int' that is the level of indentation; each level of descent
-- through a tree of 'Pred' increments this 'Int' by one.  Because the
-- function returns a list of 'Chunk', you can use multiple colors.
-- Typically this function will indent text accordingly, with a
-- newline at the end.
type Chunker = Int -> [Chunk]

-- | A rose tree of predicates.
data Pred a = Pred
  { static :: Tree Chunker
    -- ^ A tree of static names, allowing you to identify the 'Pred'
    -- without applying it to a subject.

  , evaluate :: a -> Tree Output
    -- ^ Evaluates a 'Pred' by applying it to a subject.
  }

instance Contravariant Pred where
  contramap f (Pred s e) = Pred s (e . f)

-- | The result of evaluating a 'Pred'.
data Output = Output
  { result :: Bool
  , visible :: Visible
    -- ^ Results that are not 'Visible' are not shown by the 'report'
    -- function.
  , short :: Maybe Chunker
    -- ^ Indicates whether there was a short circuit when evaluating
    -- this 'Pred'.  A short circuit occurs when the 'Pred' does not
    -- need to evaluate all of its children in order to reach a
    -- result.  If 'Nothing', there was no short circuit; otherwise,
    -- this is a 'Just' with a 'Chunker' providing a way to display
    -- the short circuit.

  , dynamic :: Chunker
    -- ^ The dynamic label; this indicates how 'report' will show the
    -- 'Pred' to the user after it has been evaluated.
  }

instance Show Output where
  show (Output r v _ _) = "output - result: " ++ show r
    ++ " visible: " ++ (show . unVisible $ v)

-- | Is this result visible?  If not, 'Prednote.report' will not show it.
newtype Visible = Visible { unVisible :: Bool }
  deriving (Eq, Ord, Show)

-- | Shown by 'Prednote.report'
shown :: Visible
shown = Visible True

-- | Hidden by 'Prednote.report'
hidden :: Visible
hidden = Visible False

-- | No 'Pred' in the list may be 'False' for 'all' to be 'True'.  An
-- empty list of 'Pred' yields a 'Pred' that always returns 'True'.
-- May short circuit.
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


-- | At least one 'Pred' in the list must be 'True' for the resulting
-- 'Pred' to be 'True'.  An empty list of 'Pred' yields a 'Pred' that
-- always returns 'False'.  May short circuit.
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


-- | Negates the child 'Pred'.  Never short circuits.
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

-- | Fanout.  May short circuit.
fan
  :: ([Bool] -> (Bool, Visible, Maybe Int))
  -- ^ This function is applied to a list of the 'result' from
  -- evaluating the child 'Pred' on each fanout item.  The function
  -- must return a triple, with the 'Bool' indicating success or
  -- failure, 'Visible' for visibility, and 'Maybe' 'Int' to indicate
  -- whether a short circuit occurred; this must be 'Nothing' if there
  -- was no short circuit, or 'Just' with an 'Int' to indicate a short
  -- circuit, with the 'Int' indicating that a short circuit occurred
  -- after examining the given number of elements.

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

-- | Fanout all.  The resulting 'Pred' is 'True' if no child item
-- returns 'False'; an empty list of child items returns 'True'.  May
-- short circuit.
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

-- | Fanout any.  The resulting 'Pred' is 'True' if at least one child
-- item returns 'True'; an empty list of child items returns 'False'.
-- May short circuit.
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

-- | Fanout at least.  The resulting 'Pred' is 'True' if at least the
-- given number of child items return 'True'.  May short circuit.
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

-- | Indents and formats output for display.
report
  :: Int
  -- ^ Start at this level of indentation.
  -> Tree Output
  -> [Chunk]
report l (Node n cs)
  | (== hidden) . visible $ n = []
  | otherwise = this ++ concatMap (report (l + 1)) cs ++ shrt
  where
    this = dynamic n l
    shrt = maybe [] ($ l) . short $ n

-- | Indents and formats static labels for display.  This is a 'plan'
-- for how the 'Pred' would be applied.
plan
  :: Int
  -- ^ Start at this level of indentation.
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

-- | Applies a 'Pred' to a single subject and returns the 'result'.
test :: Pred a -> a -> Bool
test p = result . rootLabel . evaluate p

-- | Like 'test' but also returns the accompanying 'report'.
testV :: Pred a -> a -> (Bool, [Chunk])
testV p a = (result . rootLabel $ t, report 0 t)
  where
    t = evaluate p a

-- | Like 'Prelude.filter'.
filter :: Pred a -> [a] -> [a]
filter p = Prelude.filter (test p)

-- | Like 'filter' but also returns a list of 'report', with one
-- 'report' for each list item.
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

