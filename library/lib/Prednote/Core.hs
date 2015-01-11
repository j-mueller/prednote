{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core where

import Rainbow
import Data.Functor.Contravariant
import Prelude hiding (all, any, maybe, and, or, not)
import Data.String
import Prednote.Format
import qualified System.IO as IO
import qualified Data.Text as X

newtype Label = Label [Chunk]
  deriving (Eq, Ord, Show)

newtype Condition = Condition [Chunk]
  deriving (Eq, Ord, Show)

newtype Value = Value String
  deriving (Eq, Ord, Show)

data Passed
  = PTerminal Label String Condition
  | PAnd Passed Passed
  | POr (Either Passed (Failed, Passed))
  | PNot Failed
  deriving (Eq, Ord, Show)

data Failed
  = FTerminal Label String Condition
  | FAnd (Either Failed (Passed, Failed))
  | FOr Failed Failed
  | FNot Passed
  deriving (Eq, Ord, Show)

newtype Pred a = Pred (a -> Either Failed Passed)

instance Show (Pred a) where
  show _ = "Pred"

instance Contravariant Pred where
  contramap f (Pred g) = Pred (g . f)

predicate
  :: Show a
  => String
  -- ^ Label
  -> String
  -- ^ Condition
  -> (a -> Bool)
  -> Pred a
predicate sLbl sCond p = Pred f
  where
    f a
      | p a = Right (PTerminal lbl (show a) cond)
      | otherwise = Left (FTerminal lbl (show a) cond)
    cond = Condition [fromString sCond]
    lbl = Label [fromString sLbl]

(&&&) :: Pred a -> Pred a -> Pred a
(Pred fL) &&& r = Pred f
  where
    f a = case fL a of
      Left b -> Left (FAnd (Left b))
      Right g -> case fR a of
        Left b -> Left (FAnd (Right (g, b)))
        Right g' -> Right (PAnd g g')
    Pred fR = r
infixr 3 &&&

(|||) :: Pred a -> Pred a -> Pred a
(Pred fL) ||| r = Pred f
  where
    Pred fR = r
    f a = case fL a of
      Left b -> case fR a of
        Left b' -> Left $ FOr b b'
        Right g -> Right $ POr (Right (b, g))
      Right g -> Right $ POr (Left g)
infixr 2 |||

not :: Pred a -> Pred a
not (Pred f) = Pred g
  where
    g a = case f a of
      Left b -> Right (PNot b)
      Right y -> Left (FNot y)

switch
  :: Pred a
  -> Pred b
  -> Pred (Either a b)
switch pa pb = Pred (either fa fb)
  where
    Pred fa = pa
    Pred fb = pb

resultToBool :: Either a b -> Bool
resultToBool = either (const False) (const True)

true :: Show a => Pred a
true = predicate "" "is ignored - always returns True" (const True)

false :: Show a => Pred a
false = predicate "" "is ignored - always returns False" (const False)

same :: Pred Bool
same = predicate "" "is returned" id

relabel :: String -> Pred a -> Pred a
relabel lbl (Pred f) = Pred f'
  where
    f' a = case f a of
      Left (FTerminal _ s c) -> Left (FTerminal l s c)
      Right (PTerminal _ s c) -> Right (PTerminal l s c)
      x -> x
    l = Label [fromString lbl]

any :: Pred a -> Pred [a]
any pa = contramap f (switch pConsCell pEnd)
  where
    pConsCell = contramap fst pa ||| contramap snd (any pa)
    f ls = case ls of
      [] -> Right ()
      x:xs -> Left (x, xs)
    pEnd = relabel "end of list" false

all :: Pred a -> Pred [a]
all pa = contramap f (switch pConsCell pEnd)
  where
    pConsCell = contramap fst pa &&& contramap snd (all pa)
    f ls = case ls of
      x:xs -> Left (x, xs)
      [] -> Right ()
    pEnd = relabel "end of list" true

resultToChunks :: Either Failed Passed -> [Chunk]
resultToChunks = either (failedToChunks 0) (passedToChunks 0)

passedToChunks :: Int -> Passed -> [Chunk]
passedToChunks i psd = lblLine i True cks ++ rest
  where
    (cks, rest) = case psd of
      PTerminal l s c -> (labelTerminal l s c, [])
      PAnd p1 p2 ->
        (labelAnd, passedToChunks nxt p1 ++ passedToChunks nxt p2)
      POr ei -> (labelOr, more)
        where
          more = case ei of
            Left y -> passedToChunks nxt y
            Right (n, y) -> failedToChunks nxt n ++ passedToChunks nxt y
      PNot n -> (labelNot, failedToChunks nxt n)
    nxt = succ i

failedToChunks :: Int -> Failed -> [Chunk]
failedToChunks i fld = lblLine i False cks ++ rest
  where
    (cks, rest) = case fld of
      FTerminal l s c -> (labelTerminal l s c, [])
      FAnd ei -> (labelAnd, more)
        where
          more = case ei of
            Left f' -> failedToChunks nxt f'
            Right (p', f') -> passedToChunks nxt p'
              ++ failedToChunks nxt f'
      FOr f1 f2 ->
        (labelOr, failedToChunks nxt f1 ++ failedToChunks nxt f2)
      FNot p -> (labelNot, passedToChunks nxt p)
    nxt = succ i

labelTerminal :: Label -> String -> Condition -> [Chunk]
labelTerminal (Label l) shwn (Condition c)
  = begin ++ [fromString shwn] ++ [" - "]
    ++ c
  where
    begin | empty l = []
          | otherwise = l ++ [" - "]
    empty = (== 0) . sum . map X.length . concat . map text

labelAnd :: [Chunk]
labelAnd = ["and - no child may be False"]

labelOr :: [Chunk]
labelOr = ["or - at least one child must be True"]

labelNot :: [Chunk]
labelNot = ["not - negates child"]

test :: Pred a -> a -> Bool
test (Pred f) = resultToBool . f

verboseTest :: Pred a -> a -> ([Chunk], Bool)
verboseTest (Pred f) a = (cks, res)
  where
    ei = f a
    res = resultToBool ei
    cks = resultToChunks ei

verboseTestStdout :: Pred a -> a -> IO Bool
verboseTestStdout p a = do
  t <- smartTermFromEnv IO.stdout
  let (cks, res) = verboseTest p a
  putChunks t cks
  return res

