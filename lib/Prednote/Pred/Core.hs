{-# LANGUAGE ExistentialQuantification #-}
module Prednote.Pred.Core where

import System.Console.Rainbow
import Prelude hiding (filter)
import qualified Prelude
import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.Text as X

data Tree a = Tree
  { node :: a
  , children :: [Tree a]
  } deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap f (Tree n cs) = Tree (f n) (map (fmap f) cs)

data Pred a = Pred
  { label :: Int -> [Chunk]
  , calc :: Calc a
  }

instance Contravariant Pred where
  contramap f (Pred l c) = Pred l (contramap f c)

data Shortable = Shortable
  { short :: Maybe (Int, Int -> [Chunk])
  , output :: Output
  }

data Calc a
  = Predicate (a -> Output)
  | Single (Pred a) (a -> Bool -> Output)
  | Variable [Pred a] (a -> [Bool] -> Shortable)
  | forall b. Fan (a -> [b]) (Pred b) (a -> [Bool] -> Shortable)

instance Contravariant Calc where
  contramap f c = case c of
    Predicate g -> Predicate (g . f)
    Single g h -> Single (contramap f g) (\a b -> h (f a) b)
    Variable ps g -> Variable (map (contramap f) ps) (\a b -> g (f a) b)
    Fan g p h -> Fan (g . f) p (\a b -> h (f a) b)


data Output = Output
  { visible :: Bool
  , result :: Bool
  , report :: Int -> [Chunk]
  }


-- | Display a 'Pred' without evaluating it.
display
  :: Int
  -- ^ Indentation level.  Ordinarily you will start at 0.
  -> Pred a
  -> [Chunk]
display lvl (Pred lbl c) = lbl lvl ++ case c of
  Predicate _ -> []
  Single p _ -> display (lvl + 1) p
  Variable ps _ -> concatMap (display (lvl + 1)) ps
  Fan _ p _ -> display (lvl + 1) p

instance Show (Pred a) where
  show = X.unpack . X.concat
    . map (X.concat . text)
    . display 0

evaluate
  :: Pred a
  -> a
  -> Tree (Maybe (Int -> [Chunk]), Output)
evaluate (Pred _ clc) a = case clc of
  Predicate f -> Tree (Nothing, f a) []
  Single p f ->
    let c = evaluate p a
        cRes = result . snd . node $ c
        o = f a cRes
    in Tree (Nothing, o) [c]

  Variable ps f ->
    let cs = map (flip evaluate a) ps
        bs = map (result . snd . node) cs
        shortable = f a bs
        (cs', maySS) = case short shortable of
          Nothing -> (cs, Nothing)
          Just (len, ss) -> (take len cs, Just ss)
    in Tree (maySS, output shortable) cs'

  Fan fanner p f ->
    let ps = fanner a
        cs = map (evaluate p) ps
        bs = map (result . snd . node) cs
        shortable = f a bs
        (cs', maySS) = case short shortable of
          Nothing -> (cs, Nothing)
          Just (len, ss) -> (take len cs, Just ss)
    in Tree (maySS, output shortable) cs'

test :: Pred a -> a -> Bool
test p = result . snd . node . evaluate p

filter :: Pred a -> [a] -> [a]
filter p = Prelude.filter (test p)

testV :: Int -> Pred a -> a -> ([Chunk], Bool)
testV idt p a = (render idt t, result . snd . node $ t)
  where
    t = evaluate p a

filterV :: Int -> Pred a -> [a] -> ([Chunk], [a])
filterV idt p as = (chks, rslts)
  where
    ts = map (evaluate p) as
    chks = concatMap (render idt) ts
    rslts
      = map fst
      . Prelude.filter (result . snd . node . snd)
      $ zip as ts

render
  :: Int
  -- ^ Level

  -> Tree (Maybe (Int -> [Chunk]), Output)
  -> [Chunk]
render l (Tree n cs) = lbl ++ kids ++ shrt
  where
    lbl = (report . snd $ n) l
    kids = concatMap (render (l + 1)) cs
    shrt = case fst n of
      Nothing -> []
      Just f -> f (l + 1)

-- | @shorter x y@ is True if list x is shorter than list y. Lazier
-- than taking the length of each list and comparing the results.
shorter :: [a] -> [a] -> Bool
shorter [] [] = False
shorter (_:_) [] = False
shorter [] (_:_) = True
shorter (_:xs) (_:ys) = shorter xs ys

-- | For instance,
--
-- > takeThrough odd [2,4,6,7,8] == [2,4,6,7]
takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough _ [] = []
takeThrough f (x:xs) = x : if f x then [] else takeThrough f xs

