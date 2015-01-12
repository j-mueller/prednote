{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core
  ( -- * Predicates and their creation
    Pred(..)
  , predicate

  -- * Predicate combinators
  -- ** Primitive combinators
  --
  -- | You might consider these combinators to be \"primitive\" in the
  -- sense that you can build a 'Pred' for any user-defined type by
  -- using these combinators alone, along with 'contramap'.  Use
  -- '&&&', '|||', and 'contramap' to analyze product types.  Use 'switch'
  -- and 'contramap' to analyze sum types.  For a simple example, see the
  -- source code for 'maybe', which is a simple sum type.  For more
  -- complicated examples, see the source code for 'any' and 'all', as
  -- a list is a sum type where one of the summands is a (recursive!)
  -- product type.
  , (&&&)
  , (|||)
  , not
  , switch

  -- ** Convenience combinators
  --
  -- | These were written using entirely the \"primitive\" combinators
  -- given above.
  , any
  , all
  , Nothing
  , maybe

  -- * Labeling
  , addLabel

  -- * Constant predicates
  , true
  , false
  , same

  -- * Evaluating predicates
  , test
  , verboseTest
  , verboseTestStdout

  -- * Results and converting them to 'Chunk's
  --
  -- | Usually you will not need these functions and types, as the
  -- functions and types above should meet most use cases; however,
  -- these are here so the test suites can use them, and in case you
  -- need them.
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

-- | Describes the condition; for example, for a @'Pred' 'Int'@,
-- this might be @is greater than 5@; for a @'Pred' 'String'@, this
-- might be @begins with \"Hello\"@.
newtype Condition = Condition [Chunk]
  deriving (Eq, Ord, Show)

-- | Stores the representation of a value; created using @'X.pack' '.'
-- 'show'@.
newtype Value = Value Text
  deriving (Eq, Ord, Show)

-- | Gives additional information about a particular 'Pred' to aid the
-- user when viewing the output.
newtype Label = Label Text
  deriving (Eq, Ord, Show)

-- | Any type that is accompanied by a set of labels.
data Labeled a = Labeled [Label] a
  deriving (Eq, Ord, Show)

instance Functor Labeled where
  fmap f (Labeled l a) = Labeled l (f a)

-- | A 'Pred' that returned 'True'
data Passed
  = PTerminal Value Condition
  -- ^ A 'Pred' created with 'predicate'
  | PAnd (Labeled Passed) (Labeled Passed)
  -- ^ A 'Pred' created with '&&&'
  | POr (Either (Labeled Passed) (Labeled Failed, Labeled Passed))
  -- ^ A 'Pred' created with '|||'
  | PNot (Labeled Failed)
  -- ^ A 'Pred' created with 'not'
  deriving (Eq, Ord, Show)

-- | A 'Pred' that returned 'False'
data Failed
  = FTerminal Value Condition
  -- ^ A 'Pred' created with 'predicate'
  | FAnd (Either (Labeled Failed) (Labeled Passed, Labeled Failed))
  -- ^ A 'Pred' created with '&&&'
  | FOr (Labeled Failed) (Labeled Failed)
  -- ^ A 'Pred' created with '|||'
  | FNot (Labeled Passed)
  -- ^ A 'Pred' created with 'not'
  deriving (Eq, Ord, Show)


-- | The result of processing a 'Pred'.
newtype Result = Result (Labeled (Either Failed Passed))
  deriving (Eq, Ord, Show)

-- | Returns whether this 'Result' failed or passed.
splitResult
  :: Result
  -> Either (Labeled Failed) (Labeled Passed)
splitResult (Result (Labeled l ei)) = case ei of
  Left n -> Left (Labeled l n)
  Right g -> Right (Labeled l g)

-- | Predicates.  Is an instance of 'Contravariant', which allows you
-- to change the type using 'contramap'.  Though the constructor is
-- exported, ordinarily you shouldn't need to use it; other functions
-- in this module create 'Pred' and manipulate them as needed.
newtype Pred a = Pred (a -> Result)

instance Show (Pred a) where
  show _ = "Pred"

instance Contravariant Pred where
  contramap f (Pred g) = Pred (g . f)

-- | Creates a new 'Pred'.  In @predicate cond f@, @cond@ describes
-- the condition, while @f@ gives the predicate function.  For
-- example, if @f@ is @(> 5)@, then @cond@ might be @"is greater than
-- 5"@.
predicate
  :: Show a
  => Text
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


-- | And.  Returns 'True' if both argument 'Pred' return 'True'.  Is
-- lazy in its second argment; if the first argument returns 'False',
-- the second is ignored.
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


-- | Or.  Returns 'True' if either argument 'Pred' returns 'True'.  Is
-- lazy in its second argument; if the first argument returns 'True',
-- the second argument is ignored.
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


-- | Negation.  Returns 'True' if the argument 'Pred' returns 'False'.
not :: Pred a -> Pred a
not (Pred f) = Pred g
  where
    g a = Result (Labeled [] rslt)
      where
        rslt = case splitResult $ f a of
          Left b -> Right (PNot b)
          Right y -> Left (FNot y)


-- | Uses the appropriate 'Pred' depending on the 'Either' value.  In
-- @'test' ('switch' l r) e@, the resulting 'Pred' returns the result
-- of @l@ if @e@ is 'Left' or the result of @r@ if @e@ is 'Right'.  Is
-- lazy, so the the argument 'Pred' that is not used is ignored.
switch
  :: Pred a
  -> Pred b
  -> Pred (Either a b)
switch pa pb = Pred (either fa fb)
  where
    Pred fa = pa
    Pred fb = pb

-- | Did this 'Result' pass or fail?
resultToBool :: Result -> Bool
resultToBool (Result (Labeled _ ei))
  = either (const False) (const True) ei


-- | Always returns 'True'
true :: Show a => Pred a
true = predicate "always returns True" (const True)

-- | Always returns 'False'
false :: Show a => Pred a
false = predicate "always returns False" (const False)

-- | Always returns its argument
same :: Pred Bool
same = predicate "is returned" id


-- | Adds descriptive text to a 'Pred'.  Gives useful information for
-- the user.  The label is added to the top 'Pred' in the tree; any
-- existing labels are also retained.  Labels that were added last
-- will be printed first.  For an example of this, see the source code
-- for 'any' and 'all' or the source code for "Prednote.Comparisons".
addLabel :: Text -> Pred a -> Pred a
addLabel s (Pred f) = Pred f'
  where
    f' a = Result (Labeled (Label s : ss) ei)
      where
        Result (Labeled ss ei) = f a


-- | Represents the end of a list.
data EndOfList = EndOfList

instance Show EndOfList where
  show _ = ""

-- | Like 'Prelude.any'; is 'True' if any of the list items are
-- 'True'.  An empty list returns 'False'.  Is lazy; will stop
-- processing if it encounters a 'True' item.
any :: Pred a -> Pred [a]
any pa = contramap f (switch (addLabel "cons cell" pConsCell) pEnd)
  where
    pConsCell =
      contramap fst (addLabel "head" pa)
      ||| contramap snd (addLabel "tail" (any pa))
    f ls = case ls of
      [] -> Right EndOfList
      x:xs -> Left (x, xs)
    pEnd = addLabel "end of list" $ contramap (const EndOfList) false

-- | Like 'Prelude.all'; is 'True' if none of the list items is
-- 'False'.  An empty list returns 'True'.  Is lazy; will stop
-- processing if it encouters a 'False' item.
all :: Pred a -> Pred [a]
all pa = contramap f (switch (addLabel "cons cell" pConsCell) pEnd)
  where
    pConsCell =
      contramap fst (addLabel "head" pa)
      &&& contramap snd (addLabel "tail" (all pa))
    f ls = case ls of
      x:xs -> Left (x, xs)
      [] -> Right EndOfList
    pEnd = addLabel "end of list" $ contramap (const EndOfList) true

-- | Represents 'Prelude.Nothing' of 'Maybe'.
data Nothing = CoreNothing

instance Show Nothing where
  show _ = ""

-- | Create a 'Pred' for 'Maybe'.
maybe
  :: Pred Nothing
  -- ^ What to do on 'Nothing'.  Usually you wil use 'true' or 'false'.
  -> Pred a
  -- ^ Analyzes 'Just' values.
  -> Pred (Maybe a)
maybe emp pa = contramap f
  (switch (addLabel "Nothing" emp) (addLabel "Just value" pa))
  where
    f may = case may of
      Nothing -> Left CoreNothing
      Just a -> Right a


explainAnd :: [Chunk]
explainAnd = ["(and)"]

explainOr :: [Chunk]
explainOr = ["(or)"]

explainNot :: [Chunk]
explainNot = ["(not)"]

-- | Runs a 'Pred' against a value.
test :: Pred a -> a -> Bool
test (Pred p) = either (const False) (const True)
  . splitResult . p

-- | Runs a 'Pred' against a particular value; also returns a list of
-- 'Chunk' describing the steps of evaulation.
verboseTest :: Pred a -> a -> ([Chunk], Bool)
verboseTest (Pred f) a = (cks, resultToBool rslt)
  where
    rslt = f a
    cks = resultToChunks rslt


-- | Like 'verboseTest', but results are printed to standard output.
-- Primarily for use in debugging or in a REPL.
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

-- | Append two lists of 'Chunk', with an intervening space if both
-- lists are not empty.
(<+>) :: [Chunk] -> [Chunk] -> [Chunk]
l <+> r
  | full l && full r = l <> [" "] <> r
  | otherwise = l <> r
  where
    full = Prelude.not . chunksNull

-- | Append two lists of 'Chunk', with an intervening hyphen if both
-- lists have text.
(<->) :: [Chunk] -> [Chunk] -> [Chunk]
l <-> r
  | full l && full r = l <> hyphen <> r
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
labelToChunks (Label txt) = [fromText txt]

explainTerminal :: Value -> Condition -> [Chunk]
explainTerminal (Value v) (Condition c)
  = [fromText v] <+> c

-- | Obtain a list of 'Chunk' describing the evaluation process.
resultToChunks :: Result -> [Chunk]
resultToChunks = either (failedToChunks 0) (passedToChunks 0)
  . splitResult

-- | Obtain a list of 'Chunk' describing the evaluation process.
passedToChunks
  :: Int
  -- ^ Number of levels of indentation
  -> Labeled Passed
  -> [Chunk]
passedToChunks i (Labeled l p) = this <> rest
  where
    this = spaces i <> (lblTrue <+> (labels `sep` explain)) <> newline
    labels = concat . intersperse hyphen . map labelToChunks $ l
    nextPass = passedToChunks (succ i)
    nextFail = failedToChunks (succ i)
    (explain, rest, sep) = case p of
      PTerminal v c -> (explainTerminal v c, [], (<->))
      PAnd p1 p2 -> (explainAnd, nextPass p1 <> nextPass p2, (<+>))
      POr ei -> (explainOr, more, (<+>))
        where
          more = case ei of
            Left y -> nextPass y
            Right (n, y) -> nextFail n <> nextPass y
      PNot n -> (explainNot, nextFail n, (<+>))

-- | Obtain a list of 'Chunk' describing the evaluation process.
failedToChunks
  :: Int
  -- ^ Number of levels of indentation
  -> Labeled Failed
  -> [Chunk]
failedToChunks i (Labeled l p) = this <> rest
  where
    this = spaces i <> (lblFalse <+> (labels `sep` explain)) <> newline
    labels = concat . intersperse hyphen . map labelToChunks $ l
    nextPass = passedToChunks (succ i)
    nextFail = failedToChunks (succ i)
    (explain, rest, sep) = case p of
      FTerminal v c -> (explainTerminal v c, [], (<->))
      FAnd ei -> (explainAnd, more, (<+>))
        where
          more = case ei of
            Left n -> nextFail n
            Right (y, n) -> nextPass y <> nextFail n
      FOr n1 n2 -> (explainOr, nextFail n1 <> nextFail n2, (<+>))
      FNot y -> (explainNot, nextPass y, (<+>))
