{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core where

import Rainbow
import Prelude hiding (all, any, maybe, and, or, not)
import qualified Prelude

data Static = Static [Chunk] Children
  deriving (Eq, Ord, Show)

data Children
  = Empty
  | One Static
  | Two Static Static
  deriving (Eq, Ord, Show)

data Pred a = Pred Static (a -> Out)

data Out = Out [Chunk] OutC
  deriving (Eq, Ord, Show)

data OutC

  = Terminal Bool
  -- ^ The bottom of a tree.  Produced by 'predicate'.

  | Hollow ShowInfo Out
  -- ^ The value of this result is the same as its child.  Produced by
  -- 'wrap' and 'switch'.  Has no independent visibility; if this
  -- result is shown, its child is also shown.  Whether this result is
  -- shown depends on the parent's visibility

  | Child1 Bool ShowKids Out
  -- ^ The value of this result is determined independently.  Produced
  -- by 'splitAnd', 'splitOr', 'and', 'or', and 'not'.  Children are
  -- not shown if the visibiliy is 'hidden'.

  | Child2 Bool ShowKids Out Out
  -- ^ The value of this result is determined independently.  Produced
  -- by 'splitAnd', 'splitOr', 'and', and 'or'.
  deriving (Eq, Ord, Show)

-- | Show the children of this level?
newtype ShowKids = ShowKids Bool
  deriving (Eq, Ord, Show)

showKids :: ShowKids
showKids = ShowKids True

hideKids :: ShowKids
hideKids = ShowKids False

-- | Show this level?  If not, levels underneath are still shown.
newtype ShowInfo = ShowInfo Bool
  deriving (Eq, Ord, Show)

showInfo :: ShowInfo
showInfo = ShowInfo True

hideInfo :: ShowInfo
hideInfo = ShowInfo False

outResult :: OutC -> Bool
outResult c = case c of
  Terminal b -> b
  Hollow _ (Out _ c') -> outResult c'
  Child1 b _ _ -> b
  Child2 b _ _ _ -> b


runPred :: Pred a -> a -> Bool
runPred (Pred _ f) a = let Out _ c = f a in outResult c

-- | Change the level of information shown.  Only affects 'Pred' built
-- with 'wrap' or 'switch'.
changeInfo :: ShowInfo -> Pred a -> Pred a
changeInfo si pd = Pred lbl f'
  where
    Pred lbl f = pd
    f' a = Out cks o'
      where
        Out cks o = f a
        o' = case o of
          Hollow _ out -> Hollow si out
          x -> x

-- | Change a 'Pred' so it is 'showInfo'.  Only affects 'Pred' built
-- with 'wrap' or 'switch'.
inform :: Pred a -> Pred a
inform = changeInfo showInfo

-- | Change a 'Pred' so it is 'hideInfo'.  Only affects 'Pred' built
-- with 'wrap' or 'switch'.
obscure :: Pred a -> Pred a
obscure = changeInfo hideInfo

-- | Set visibility of children depending on whether the 'Pred' is
-- 'True' or 'False'.  Does not affect 'Pred' built with 'predicate',
-- 'wrap', or 'switch'.

visibility :: (Bool -> ShowKids) -> Pred a -> Pred a
visibility fVis (Pred lbl f) = Pred lbl f'
  where
    f' a = Out cks o'
      where
        Out cks o = f a
        o' = case o of
          Child1 r _ c -> Child1 r (fVis r) c
          Child2 r _ c1 c2 -> Child2 r (fVis r) c1 c2
          x -> x


showTrue :: Pred a -> Pred a
showTrue = visibility f
  where
    f b | b = showKids
        | otherwise = hideKids

showFalse :: Pred a -> Pred a
showFalse = visibility f
  where
    f b | Prelude.not b = showKids
        | otherwise = hideKids

hideTrue :: Pred a -> Pred a
hideTrue = showFalse

hideFalse :: Pred a -> Pred a
hideFalse = showTrue

instance Show (Pred a) where
  show _ = "Pred"


test :: Pred a -> a -> Bool
test (Pred _ f) a = let Out _ c = f a in outResult c


data Annotated a = Annotated [Chunk] a

instance Show (Annotated a) where
  show (Annotated cks _) = "Annotated " ++ show cks

instance Functor Annotated where
  fmap f (Annotated c a) = Annotated c (f a)


predicate
  :: [Chunk]
  -- ^ Static label
  -> (a -> Annotated Bool)
  -> Pred a
predicate lbl f = Pred (Static lbl Empty) f'
  where
    f' a = Out cks (Terminal r)
      where
        Annotated cks r = f a


true :: Pred a
true = predicate lbl (const (Annotated lbl True))
  where
    lbl = [fromText "always returns True"]

false :: Pred a
false = predicate lbl (const (Annotated lbl False))
  where
    lbl = [fromText "always returns False"]

same :: Pred Bool
same = predicate lbl (Annotated lbl)
  where
    lbl = [fromText "returns its argument"]

wrap
  :: [Chunk]
  -> (a -> Annotated b)
  -> Pred b
  -> Pred a
wrap st spawn (Pred lbl f) = Pred lbl' f'
  where
    lbl' = Static st (One lbl)
    f' a = Out ann res
      where
        Annotated ann b = spawn a
        res = Hollow showInfo (f b)


switch
  :: [Chunk]
  -> (a -> Annotated (Either b c))
  -> Pred b
  -> Pred c
  -> Pred a
switch st split pB pC = Pred lbl' f
  where
    Pred lblB fB = pB
    Pred lblC fC = pC
    lbl' = Static st (Two lblB lblC)
    f a = Out ann (Hollow showInfo child)
      where
        Annotated ann ei = split a
        child = case ei of
          Left b -> fB b
          Right c -> fC c


combiningPred
  :: (Bool -> Bool)
  -- ^ What to do to the leftmost result
  -> (Bool -> Bool -> Bool)
  -- ^ How to obtain a result if short-circuiting fails
  -> [Chunk]
  -- ^ Static name
  -> (a -> [Chunk])
  -- ^ Obtains dynamic name
  -> Pred a
  -- ^ Left-hand side
  -> Pred a
  -- ^ Right-hand side
  -> Pred a
combiningPred chLeft comb st fDyn pA pB = Pred lbls f
  where
    lbls = Static st (Two lblA lblB)
    Pred lblA fA = pA
    Pred lblB fB = pB
    f a = Out dyn c
      where
        dyn = fDyn a
        outA@(Out _ oA) = fA a
        outB@(Out _ oB) = fB a
        resA = outResult oA
        resB = outResult oB
        c | chLeft resA = Child1 resA showKids outA
          | otherwise = Child2 (resA `comb` resB) showKids outA outB


and
  :: [Chunk]
  -> (a -> [Chunk])
  -> Pred a
  -> Pred a
  -> Pred a
and = combiningPred Prelude.not (&&)

or
  :: [Chunk]
  -> (a -> [Chunk])
  -> Pred a
  -> Pred a
  -> Pred a
or = combiningPred id (||)

not
  :: [Chunk]
  -> (a -> [Chunk])
  -> Pred a
  -> Pred a
not st fDyn (Pred lbl f) = Pred lbl' f'
  where
    lbl' = Static st (One lbl)
    f' a = Out (fDyn a) (Child1 res showKids child)
      where
        child@(Out _ c) = f a
        res = Prelude.not . outResult $ c
