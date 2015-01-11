{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core where

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

data Passed
  = PTerminal Value Condition
  | PAnd Passed Passed
  | POr (Either Passed (Failed, Passed))
  | PNot Failed
  deriving (Eq, Ord, Show)

data Failed
  = FTerminal Value Condition
  | FAnd (Either Failed (Passed, Failed))
  | FOr Failed Failed
  | FNot Passed
  deriving (Eq, Ord, Show)

data Result = Result [Label] (Either Failed Passed)
  deriving (Eq, Ord, Show)

failedOrPassed :: Result -> Either Failed Passed
failedOrPassed (Result _ r) = r

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
    f a = Result [] r
      where
        r | p a = Right (PTerminal val cond)
          | otherwise = Left (FTerminal val cond)
        cond = Condition [fromText tCond]
        val = Value . X.pack . show $ a


(&&&) :: Pred a -> Pred a -> Pred a
(Pred fL) &&& r = Pred f
  where
    f a = Result [] rslt
      where
        rslt = case failedOrPassed $ fL a of
          Left b -> Left (FAnd (Left b))
          Right g -> case failedOrPassed $ fR a of
            Left b -> Left (FAnd (Right (g, b)))
            Right g' -> Right (PAnd g g')
        Pred fR = r
infixr 3 &&&

(|||) :: Pred a -> Pred a -> Pred a
(Pred fL) ||| r = Pred f
  where
    Pred fR = r
    f a = Result [] rslt
      where
        rslt = case failedOrPassed $ fL a of
          Left b -> case failedOrPassed $ fR a of
            Left b' -> Left $ FOr b b'
            Right g -> Right $ POr (Right (b, g))
          Right g -> Right $ POr (Left g)
infixr 2 |||

not :: Pred a -> Pred a
not (Pred f) = Pred g
  where
    g a = Result [] rslt
      where
        rslt = case failedOrPassed $ f a of
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
resultToBool (Result _ ei) = either (const False) (const True) ei

true :: Show a => Pred a
true = predicate "is ignored - always returns True" (const True)

false :: Show a => Pred a
false = predicate "is ignored - always returns False" (const False)

same :: Pred Bool
same = predicate "is returned" id

addLabel :: Text -> Pred a -> Pred a
addLabel s (Pred f) = Pred f'
  where
    f' a = Result (Label s : ss) ei
      where
        Result ss ei = f a

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


{-
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

-}

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

-- | Append two lists of 'Chunk' with an intervening hyphen if both
-- lists are not zero length.
(<->) :: [Chunk] -> [Chunk] -> [Chunk]
l <-> r
  | full l && full r = l <> [" - "] <> r
  | otherwise = l <> r
  where
    full = Prelude.not . chunksNull

hyphen :: [Chunk]
hyphen = [" - "]

-- | Length of a list of 'Chunk' in characters.
chunksLength :: [Chunk] -> Int
chunksLength = sum . map X.length . concat . map text

chunksNull :: [Chunk] -> Bool
chunksNull = Prelude.all $ Prelude.all X.null . text

indentAmt :: Int
indentAmt = 2

spaces :: Int -> [Chunk]
spaces i = (:[]) . fromText . X.replicate i . X.singleton $ ' '

passedToChunks :: Int -> Passed -> ([Chunk], [Chunk])
passedToChunks = undefined
{-
passedToChunks i p = case p of
  PTerminal v c -> (terminalToChunks v c, [])
  PAnd p1 p2 ->
    (["and (both children must be True)"], nextPass p1 <> nextPass p2)
  POr ei -> (["or (either child must be True)"], rest)
    where
      rest = either nextPass
        (\(n, y) -> nextFail n <> nextPass y) ei
  PNot n -> (["not (child must be False)"], nextFail n)
  where
    nextFail = failedToChunks (i + 1)
    nextPass = passedToChunks (i + 1)
-}
failedToChunks :: Int -> Failed -> ([Chunk], [Chunk])
failedToChunks = undefined

newline :: [Chunk]
newline = ["\n"]

resultToChunks :: Int -> Result -> [Chunk]
resultToChunks i (Result lbl ei) = this <> rest
  where
    this = spaces i <> ((trueFalse <+> labels) <-> end) <> newline
    trueFalse = either (const lblFalse) (const lblTrue) ei
    labels = concat . intersperse hyphen . map labelToChunks $ lbl
    (end, rest) = either (failedToChunks i) (passedToChunks i) ei
      

labelToChunks :: Label -> [Chunk]
labelToChunks (Label txt) = [fore yellow <> fromText txt]

terminalToChunks :: Value -> Condition -> [Chunk]
terminalToChunks (Value v) (Condition c) = [fromText v] <+> c
