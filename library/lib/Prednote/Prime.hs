module Prednote.Prime where

import Rainbow
import Prelude hiding (all, any, maybe, and, or)
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

evaluate :: Pred a -> a -> Tree Output
evaluate (Pred _ f) a = f a

instance Contravariant Pred where
  contramap f (Pred t e) = Pred t (e . f)

newtype Short = Short Bool
  deriving (Eq, Ord, Show)

short :: Short
short = Short True

full :: Short
full = Short False

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

and
  :: [Chunk]
  -- ^ Static label
  -> (a -> Annotated (b, c))
  -> Pred b
  -> Pred c
  -> Pred a
and st spawn (Pred lblB fB) (Pred lblC fC) = Pred lbls f
  where
    lbls = Tree st (Two lblB lblC)
    f a = Tree out children
      where
        out = Output res shown dynL
        Annotated dynL (b, c) = spawn a
        outB@(Tree (Output resB _ _) _) = fB b
        outC@(Tree (Output resC _ _) _) = fC c
        (children, res)
          | not resB = (One outB, False)
          | otherwise = (Two outB outC, resB && resC)

or
  :: [Chunk]
  -- ^ Static label
  -> (a -> Annotated (b, c))
  -> Pred b
  -> Pred c
  -> Pred a
or st spawn (Pred lblB fB) (Pred lblC fC) = Pred lbls f
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


switch
  :: [Chunk]
  -> (a -> Annotated (Either b c))
  -> Pred b
  -> Pred c
  -> Pred a
switch st split (Pred lblB fB) (Pred lblC fC) = Pred lbls f
  where
    
    Annotated dynL ei = split 

{-
and :: (a -> (b, c)) -> (b -> Bool) -> (c -> Bool) -> a -> Bool
and prod fb fc a = fb b && fc c
  where
    (b, c) = prod a


or :: (a -> (b, c)) -> (b -> Bool) -> (c -> Bool) -> a -> Bool
or prod fb fc a = fb b || fc c
  where
    (b, c) = prod a

switch :: (a -> Either b c) -> (b -> Bool) -> (c -> Bool) -> a -> Bool
switch switcher fb fc = either fb fc . switcher

false :: a -> Bool
false = const False

true :: a -> Bool
true = const True

all :: (a -> Bool) -> [a] -> Bool
all f = switch caser true fPair
  where
    caser [] = Left ()
    caser (x:xs) = Right (x, xs)
    fPair = and id f (all f)

any :: (a -> Bool) -> [a] -> Bool
any f = switch caser false fPair
  where
    caser [] = Left ()
    caser (x:xs) = Right (x, xs)
    fPair = or id f (any f)

maybe :: (a -> Bool) -> Maybe a -> Bool
maybe f = switch caser false f
  where
    caser Nothing = Left ()
    caser (Just x) = Right x

all3 :: (a -> Bool) -> (b -> Bool) -> (c -> Bool) -> (a, b, c) -> Bool
all3 fa fb fc = and mkTup fa fbc
  where
    mkTup (a, b, c) = (a, (b, c))
    fbc = and id fb fc

data S3 = S3a Int | S3b Char | S3c String

anyS3 :: (Int -> Bool) -> (Char -> Bool) -> (String -> Bool) -> S3 -> Bool
anyS3 fInt fChar fString = switch switch1 fInt (switch id fChar fString)
  where
    switch1 s3 = case s3 of
      S3a i -> Left i
      S3b c -> Right (Left c)
      S3c s -> Right (Right s)

data S4 = S4a Int | S4b Char | S4c String | S4d (Maybe Int)

anyS4 :: (Int -> Bool) -> (Char -> Bool) -> (String -> Bool) -> (Maybe Int -> Bool)
  -> S4 -> Bool

anyS4 fI fC fS fM = switch switch1 fI (switch id fC (switch id fS fM))
  where
    switch1 s4 = case s4 of
      S4a i -> Left i   
      S4b c -> Right (Left c)
      S4c s -> Right (Right (Left s))
      S4d m -> Right (Right (Right m))
-}
