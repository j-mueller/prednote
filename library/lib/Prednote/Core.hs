module Prednote.Core where

import Rainbow
import Prelude hiding (all, any, maybe, and, or, not)
import qualified Prelude
import Data.Functor.Contravariant (Contravariant(..))

data Tree a = Tree a (Children a)
  deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap f (Tree a c) = Tree (f a) (fmap f c)

data Children a
  = Empty
  | One (Tree a)
  | Two (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

instance Functor Children where
  fmap _ Empty = Empty
  fmap f (One t) = One (fmap f t)
  fmap f (Two x y) = Two (fmap f x) (fmap f y)

-- | Is this result visible?  If not, 'Prednote.report' will not show it.
newtype Visible = Visible Bool
  deriving (Eq, Ord, Show)

shown :: Visible
shown = Visible True

hidden :: Visible
hidden = Visible False

data Pred a = Pred (Tree [Chunk]) (a -> Tree Output)

test :: Pred a -> a -> Bool
test (Pred _ f) a = let (Tree (Output res _ _) _) = f a in res

instance Contravariant Pred where
  contramap f (Pred t e) = Pred t (e . f)

data Output = Output Bool Visible [Chunk]
  deriving (Eq, Ord, Show)

data Annotated a = Annotated [Chunk] a

instance Functor Annotated where
  fmap f (Annotated c a) = Annotated c (f a)

predicate
  :: [Chunk]
  -- ^ Static label
  -> (a -> Annotated Bool)
  -> Pred a
predicate lbl f = Pred (Tree lbl Empty) f'
  where
    f' a = Tree out Empty
      where
        out = Output res shown ann
        Annotated ann res = f a


splitAnd
  :: [Chunk]
  -- ^ Static label
  -> (a -> Annotated (b, c))
  -> Pred b
  -> Pred c
  -> Pred a
splitAnd st spawn (Pred lblB fB) (Pred lblC fC) = Pred lbls f
  where
    lbls = Tree st (Two lblB lblC)
    f a = Tree out children
      where
        out = Output res shown dynL
        Annotated dynL (b, c) = spawn a
        outB@(Tree (Output resB _ _) _) = fB b
        outC@(Tree (Output resC _ _) _) = fC c
        (children, res)
          | Prelude.not resB = (One outB, False)
          | otherwise = (Two outB outC, resB && resC)


splitOr
  :: [Chunk]
  -- ^ Static label
  -> (a -> Annotated (b, c))
  -> Pred b
  -> Pred c
  -> Pred a
splitOr st spawn (Pred lblB fB) (Pred lblC fC) = Pred lbls f
  where
    lbls = Tree st (Two lblB lblC)
    f a = Tree out children
      where
        out = Output res shown dynL
        Annotated dynL (b, c) = spawn a
        outB@(Tree (Output resB _ _) _) = fB b
        outC@(Tree (Output resC _ _) _) = fC c
        (children, res)
          | resB = (One outB, True)
          | otherwise = (Two outB outC, resB || resC)


wrap
  :: [Chunk]
  -> (a -> Annotated b)
  -> Pred b
  -> Pred a
wrap st spawn (Pred lbl f) = Pred lbl' f'
  where
    lbl' = Tree st (One lbl)
    f' a = Tree out (One chld)
      where
        Annotated dynL b = spawn a
        chld@(Tree (Output chldRes _ _) _) = f b
        out = Output chldRes shown dynL


switch
  :: [Chunk]
  -> (a -> Annotated (Either b c))
  -> Pred b
  -> Pred c
  -> Pred a
switch st split (Pred lblB fB) (Pred lblC fC) = Pred lbls f
  where
    lbls = Tree st (Two lblB lblC)
    f a = Tree out (One child)
      where
        Annotated dynL ei = split a
        out = Output res shown dynL
        child@(Tree (Output res _ _) _) = case ei of
          Left b -> fB b
          Right c -> fC c


and
  :: [Chunk]
  -> (a -> [Chunk])
  -> Pred a
  -> Pred a
  -> Pred a
and st fDyn (Pred lblA fA) (Pred lblB fB) = Pred lbls f
  where
    lbls = Tree st (Two lblA lblB)
    f a = Tree out children
      where
        out = Output res shown (fDyn a)
        outA@(Tree (Output resA _ _) _) = fA a
        outB@(Tree (Output resB _ _) _) = fB a
        (children, res)
          | Prelude.not resA = (One outA, False)
          | otherwise = (Two outA outB, resA && resB)


or
  :: [Chunk]
  -> (a -> [Chunk])
  -> Pred a
  -> Pred a
  -> Pred a
or st fDyn (Pred lblA fA) (Pred lblB fB) = Pred lbls f
  where
    lbls = Tree st (Two lblA lblB)
    f a = Tree out children
      where
        out = Output res shown (fDyn a)
        outA@(Tree (Output resA _ _) _) = fA a
        outB@(Tree (Output resB _ _) _) = fB a
        (children, res)
          | resA = (One outA, True)
          | otherwise = (Two outA outB, resA || resB)


not
  :: [Chunk]
  -> (a -> [Chunk])
  -> Pred a
  -> Pred a
not st fDyn (Pred lbl f) = Pred lbl' f'
  where
    lbl' = Tree st (One lbl)
    f' a = Tree out (One chld)
      where
        chld@(Tree (Output chldRes _ _) _) = f a
        out = Output (Prelude.not chldRes) shown (fDyn a)

