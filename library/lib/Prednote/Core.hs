{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core
  ( -- * Predicates and their creation
    Pred(..)
  , predicate

  -- * Predicate combinators
  , (&&&)
  , (|||)
  , not
  , switch
  , any
  , all

  -- * Constant predicates
  , true
  , false
  , same

  -- * Evaluating predicates
  , test
  , verboseTest
  , verboseTestStdout

  -- * Results and converting them to 'Chunk's
  , Condition(..)
  , Value(..)
  , Label(..)
  , Labeled(..)
  , Passed(..)
  , Failed(..)
  , Result(..)
  , splitResult
  , resultToChunks
  , passedToChunks
  , failedToChunks
  ) where

import Rainbow
import Data.Monoid
import Data.Functor.Contravariant
import Prelude hiding (all, any, maybe, and, or, not)
import qualified Prelude
import qualified System.IO as IO
import qualified Data.Text as X
import Data.Text (Text)
import Data.List (intersperse)

newtype Condition = Condition [Chunk]
  deriving (Eq, Ord, Show)

newtype Value = Value Text
  deriving (Eq, Ord, Show)

newtype Label = Label Text
  deriving (Eq, Ord, Show)

data Labeled a = Labeled [Label] a
  deriving (Eq, Ord, Show)

instance Functor Labeled where
  fmap f (Labeled l a) = Labeled l (f a)

data Passed
  = PTerminal Value Condition
  | PAnd (Labeled Passed) (Labeled Passed)
  | POr (Either (Labeled Passed) (Labeled Failed, Labeled Passed))
  | PNot (Labeled Failed)
  deriving (Eq, Ord, Show)

data Failed
  = FTerminal Value Condition
  | FAnd (Either (Labeled Failed) (Labeled Passed, Labeled Failed))
  | FOr (Labeled Failed) (Labeled Failed)
  | FNot (Labeled Passed)
  deriving (Eq, Ord, Show)

newtype Result = Result (Labeled (Either Failed Passed))
  deriving (Eq, Ord, Show)

splitResult
  :: Result
  -> Either (Labeled Failed) (Labeled Passed)
splitResult (Result (Labeled l ei)) = case ei of
  Left n -> Left (Labeled l n)
  Right g -> Right (Labeled l g)

newtype Pred a = Pred (a -> Result)

instance Show (Pred a) where
  show _ = "Pred"

instance Contravariant Pred where
  contramap f (Pred g) = Pred (g . f)

predicate
  :: Show a
  => Text
  -- ^ Condition
  -> (a -> Bool)
  -> Pred a
predicate tCond p = Pred f
  where
    f a = Result (Labeled [] r)
      where
        r | p a = Right (PTerminal val cond)
          | otherwise = Left (FTerminal val cond)
        cond = Condition [fromText tCond]
        val = Value . X.pack . show $ a


(&&&) :: Pred a -> Pred a -> Pred a
(Pred fL) &&& r = Pred f
  where
    f a = Result (Labeled [] rslt)
      where
        rslt = case splitResult $ fL a of
          Left n -> Left (FAnd (Left n))
          Right g -> case splitResult $ fR a of
            Left b -> Left (FAnd (Right (g, b)))
            Right g' -> Right (PAnd g g')
        Pred fR = r
infixr 3 &&&


(|||) :: Pred a -> Pred a -> Pred a
(Pred fL) ||| r = Pred f
  where
    Pred fR = r
    f a = Result (Labeled [] rslt)
      where
        rslt = case splitResult $ fL a of
          Left b -> case splitResult $ fR a of
            Left b' -> Left $ FOr b b'
            Right g -> Right $ POr (Right (b, g))
          Right g -> Right $ POr (Left g)
infixr 2 |||


not :: Pred a -> Pred a
not (Pred f) = Pred g
  where
    g a = Result (Labeled [] rslt)
      where
        rslt = case splitResult $ f a of
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

resultToBool :: Result -> Bool
resultToBool (Result (Labeled _ ei))
  = either (const False) (const True) ei


true :: Show a => Pred a
true = predicate "is ignored - always returns True" (const True)

false :: Show a => Pred a
false = predicate "is ignored - always returns False" (const False)

same :: Pred Bool
same = predicate "is returned" id


addLabel :: Text -> Pred a -> Pred a
addLabel s (Pred f) = Pred f'
  where
    f' a = Result (Labeled (Label s : ss) ei)
      where
        Result (Labeled ss ei) = f a


any :: Pred a -> Pred [a]
any pa = contramap f (switch pConsCell pEnd)
  where
    pConsCell = contramap fst pa ||| contramap snd (any pa)
    f ls = case ls of
      [] -> Right ()
      x:xs -> Left (x, xs)
    pEnd = addLabel "end of list" false

all :: Pred a -> Pred [a]
all pa = contramap f (switch pConsCell pEnd)
  where
    pConsCell = contramap fst pa &&& contramap snd (all pa)
    f ls = case ls of
      x:xs -> Left (x, xs)
      [] -> Right ()
    pEnd = addLabel "end of list" true

explainAnd :: [Chunk]
explainAnd = ["and (no child may be False)"]

explainOr :: [Chunk]
explainOr = ["or (at least one child must be True)"]

explainNot :: [Chunk]
explainNot = ["not (negates child)"]

test :: Pred a -> a -> Bool
test (Pred p) = either (const False) (const True)
  . splitResult . p

verboseTest :: Pred a -> a -> ([Chunk], Bool)
verboseTest (Pred f) a = (cks, resultToBool rslt)
  where
    rslt = f a
    cks = resultToChunks rslt


verboseTestStdout :: Pred a -> a -> IO Bool
verboseTestStdout p a = do
  let (cks, r) = verboseTest p a
  t <- smartTermFromEnv IO.stdout
  putChunks t cks
  return r

-- | A colorful label for 'True' values.
lblTrue :: [Chunk]
lblTrue = ["[", fore green <> "TRUE", "]"]

-- | A colorful label for 'False' values.
lblFalse :: [Chunk]
lblFalse = ["[", fore red <> "FALSE", "]"]

-- | Append two lists of 'Chunk', with an intervening space if both 'String'
-- are not empty.
(<+>) :: [Chunk] -> [Chunk] -> [Chunk]
l <+> r
  | full l && full r = l <> [" "] <> r
  | otherwise = l <> r
  where
    full = Prelude.not . chunksNull

hyphen :: [Chunk]
hyphen = [" - "]

chunksNull :: [Chunk] -> Bool
chunksNull = Prelude.all $ Prelude.all X.null . text

indentAmt :: Int
indentAmt = 2

spaces :: Int -> [Chunk]
spaces i = (:[]) . fromText . X.replicate (i * indentAmt)
  . X.singleton $ ' '

newline :: [Chunk]
newline = ["\n"]

labelToChunks :: Label -> [Chunk]
labelToChunks (Label txt) = [fore yellow <> fromText txt]

explainTerminal :: Value -> Condition -> [Chunk]
explainTerminal (Value v) (Condition c) = [fromText v] <+> c

resultToChunks :: Result -> [Chunk]
resultToChunks = either (failedToChunks 0) (passedToChunks 0)
  . splitResult

passedToChunks
  :: Int
  -> Labeled Passed
  -> [Chunk]
passedToChunks i (Labeled l p) = this <> rest
  where
    this = spaces i <> ((lblTrue <+> labels) <+> explain) <> newline
    labels = concat . intersperse hyphen . map labelToChunks $ l
    nextPass = passedToChunks (succ i)
    nextFail = failedToChunks (succ i)
    (explain, rest) = case p of
      PTerminal v c -> (explainTerminal v c, [])
      PAnd p1 p2 -> (explainAnd, nextPass p1 <> nextPass p2)
      POr ei -> (explainOr, more)
        where
          more = case ei of
            Left y -> nextPass y
            Right (n, y) -> nextFail n <> nextPass y
      PNot n -> (explainNot, nextFail n)

failedToChunks
  :: Int
  -> Labeled Failed
  -> [Chunk]
failedToChunks i (Labeled l p) = this <> rest
  where
    this = spaces i <> ((lblFalse <+> labels) <+> explain) <> newline
    labels = concat . intersperse hyphen . map labelToChunks $ l
    nextPass = passedToChunks (succ i)
    nextFail = failedToChunks (succ i)
    (explain, rest) = case p of
      FTerminal v c -> (explainTerminal v c, [])
      FAnd ei -> (explainAnd, more)
        where
          more = case ei of
            Left n -> nextFail n
            Right (y, n) -> nextPass y <> nextFail n
      FOr n1 n2 -> (explainOr, nextFail n1 <> nextFail n2)
      FNot y -> (explainNot, nextPass y)
