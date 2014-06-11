{-# LANGUAGE OverloadedStrings #-}
module Prednote.Monadic.Predbox where

import System.Console.Rainbow
import Data.Monoid
import Control.Applicative
import Control.Monad
import qualified Data.Text as X
import Prelude hiding (and, or, not)
import qualified Prelude

newtype Message = Message { unMessage :: [Chunk] }
  deriving (Eq, Ord, Show)

instance Monoid Message where
  mempty = Message []
  mappend (Message x) (Message y) = Message $ x ++ y

data Tree = Tree
  { this :: Message
  , children :: [Tree]
  } deriving (Eq, Ord, Show)

data Predbox a = Predbox
  { messages :: [Tree]
  , calc :: a
  } deriving (Eq, Ord, Show)

instance Functor Predbox where
  fmap f (Predbox t a) = Predbox t (f a)

instance Monad Predbox where
  return = Predbox mempty
  (Predbox t1 a) >>= f =
    let Predbox t2 b = f a
    in Predbox (t1 <> t2) b

instance Applicative Predbox where
  pure = return
  (<*>) = ap

passFail :: Bool -> [Chunk]
passFail b = ["[", l, "]"]
  where
    l | b = "PASS" <> f_green
      | otherwise = "FAIL" <> f_red

predicate
  :: String
  -- ^ Label for this predicate
  -> (a -> Bool)
  -- ^ Predicate to run
  -> a
  -> Predbox Bool
  -- ^ Resulting computation
predicate l p a = Predbox ([Tree m []]) r
  where
    m = Message $ passFail r <> [" ", fromText . X.pack $ l]
    r = p a

and
  :: String
  -- ^ Label
  -> [a -> Predbox Bool]
  -> a
  -> Predbox Bool
and l ls a = Predbox ([Tree m cs]) r
  where
    m = Message $ passFail r <> [" ", fromText . X.pack $ l]
    cs = reverse revdTrees
    (revdTrees, r) = go [] ls
    go soFar rs = case rs of
      [] -> (soFar, True)
      x:xs ->
        let Predbox cdren cr = x a
        in case () of
            _ | cr -> go (cdren:soFar) xs
              | otherwise -> (cdren:soFar, False)

or
  :: String
  -- ^ Label
  -> [a -> Predbox Bool]
  -> a
  -> Predbox Bool
or l ls a = Predbox ([Tree m cs]) r
  where
    m = Message $ passFail r <> [" ", fromText . X.pack $ l]
    cs = reverse revdTrees
    (revdTrees, r) = go [] ls
    go soFar rs = case rs of
      [] -> (soFar, False)
      x:xs ->
        let Predbox cdren cr = x a
        in case () of
            _ | Prelude.not cr -> go (cdren:soFar) xs
              | otherwise -> (cdren:soFar, True)

not
  :: String
  -- ^ Label
  -> (a -> Predbox Bool)
  -> a
  -> Predbox Bool
not l fn a = Predbox ([Tree m [cdren]]) r
  where
    m = Message $ passFail r <> [" ", fromText . X.pack $ l]
    Predbox cdren bl = fn a
    r = Prelude.not bl
