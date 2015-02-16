{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core
  ( -- * Predicates and their creation
    PredM(..)
  , Pred
  , predicate
  , predicateM

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
  , testM
  , runPred
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
import Data.Functor.Identity
import Control.Applicative

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
-- in this module create 'PredM' and manipulate them as needed.
--
-- The @f@ type variable is an arbitrary context; ordinarily this type
-- will be an instance of 'Monad', and some of the bindings in this
-- module require this.  That allows you to run predicate computations
-- that run in some sort of context, allowing you to perform IO,
-- examine state, or whatever.  If you only want to do pure
-- computations, just use the 'Pred' type synonym.
newtype PredM f a = PredM { runPredM :: (a -> f Result) }

-- | Predicates that do not run in any context.
type Pred = PredM Identity

-- | Runs pure 'Pred' computations.
runPred :: Pred a -> a -> Result
runPred (PredM f) a = runIdentity $ f a

instance Show (PredM f a) where
  show _ = "Pred"

instance Contravariant (PredM f) where
  contramap f (PredM g) = PredM (g . f)

-- | Creates a new 'PredM' that run in some arbitrary context.  In
-- @predicateM cond f@, @cond@ describes the condition, while @f@
-- gives the predicate function.  For example, if @f@ is @(> 5)@, then
-- @cond@ might be @"is greater than 5"@.
predicateM
  :: (Show a, Functor f)
  => Text
  -> (a -> f Bool)
  -- ^ Predicate function; this is in an arbitrary context, allowing
  -- you to perform IO, examine and change state, etc.  If you do not
  -- need to use a context, see 'predicate'.
  -> PredM f a
predicateM tCond p = PredM f
  where
    f a = fmap mkResult $ p a
      where
        mkResult b = Result (Labeled [] r)
          where
            r | b = Right (PTerminal val cond)
              | otherwise = Left (FTerminal val cond)
            cond = Condition [fromText tCond]
            val = Value . X.pack . show $ a

-- | Creates a new 'Pred' that do not run in any context.  In
-- @predicate cond f@, @cond@ describes the condition, while @f@ gives
-- the predicate function.  For example, if @f@ is @(> 5)@, then
-- @cond@ might be @"is greater than 5"@.
predicate
  :: Show a
  => Text
  -> (a -> Bool)
  -> Pred a
predicate lbl f = predicateM lbl (fmap return f)

-- | And.  Returns 'True' if both argument 'Pred' return 'True'.  Is
-- lazy in its second argment; if the first argument returns 'False',
-- the second is ignored.
(&&&) :: Monad m => PredM m a -> PredM m a -> PredM m a
(PredM fL) &&& r = PredM $ \a -> do
  resL <- fL a
  ei <- case splitResult resL of
    Left n -> return (Left (FAnd (Left n)))
    Right g -> do
      let PredM fR = r
      resR <- fR a
      return $ case splitResult resR of
        Left b -> Left (FAnd (Right (g, b)))
        Right g' -> Right (PAnd g g')
  return (Result (Labeled [] ei))

infixr 3 &&&


-- | Or.  Returns 'True' if either argument 'Pred' returns 'True'.  Is
-- lazy in its second argument; if the first argument returns 'True',
-- the second argument is ignored.
(|||) :: Monad m => PredM m a -> PredM m a -> PredM m a
(PredM fL) ||| r = PredM $ \a -> do
  resL <- fL a
  ei <- case splitResult resL of
    Left b -> do
      let PredM fR = r
      resR <- fR a
      return $ case splitResult resR of
        Left b' -> Left $ FOr b b'
        Right g -> Right $ POr (Right (b, g))
    Right g -> return (Right (POr (Left g)))
  return (Result (Labeled [] ei))  
infixr 2 |||

-- | Negation.  Returns 'True' if the argument 'Pred' returns 'False'.
not :: Functor m => PredM m a -> PredM m a
not (PredM f) = PredM $ \a -> fmap g (f a)
  where
    g a = Result (Labeled [] rslt)
      where
        rslt = case splitResult a of
          Left b -> Right (PNot b)
          Right y -> Left (FNot y)


-- | Uses the appropriate 'Pred' depending on the 'Either' value.  In
-- @'test' ('switch' l r) e@, the resulting 'Pred' returns the result
-- of @l@ if @e@ is 'Left' or the result of @r@ if @e@ is 'Right'.  Is
-- lazy, so the the argument 'Pred' that is not used is ignored.
switch
  :: PredM m a
  -> PredM m b
  -> PredM m (Either a b)
switch pa pb = PredM (either fa fb)
  where
    PredM fa = pa
    PredM fb = pb

-- | Did this 'Result' pass or fail?
resultToBool :: Result -> Bool
resultToBool (Result (Labeled _ ei))
  = either (const False) (const True) ei


-- | Always returns 'True'
true :: (Show a, Applicative f) => PredM f a
true = predicateM "always returns True" (const (pure True))

-- | Always returns 'False'
false :: (Show a, Applicative f) => PredM f a
false = predicateM "always returns False" (const (pure False))

-- | Always returns its argument
same :: Applicative f => PredM f Bool
same = predicateM "is returned" (pure . id)

-- | Adds descriptive text to a 'Pred'.  Gives useful information for
-- the user.  The label is added to the top 'Pred' in the tree; any
-- existing labels are also retained.  Labels that were added last
-- will be printed first.  For an example of this, see the source code
-- for 'any' and 'all'.
addLabel :: Functor f => Text -> PredM f a -> PredM f a
addLabel s (PredM f) = PredM f'
  where
    f' a = fmap g (f a)
      where
        g (Result (Labeled ss ei)) = Result (Labeled (Label s : ss) ei)


-- | Represents the end of a list.
data EndOfList = EndOfList

instance Show EndOfList where
  show _ = ""

-- | Like 'Prelude.any'; is 'True' if any of the list items are
-- 'True'.  An empty list returns 'False'.  Is lazy; will stop
-- processing if it encounters a 'True' item.
any :: (Functor m, Monad m, Applicative m) => PredM m a -> PredM m [a]
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
all :: (Functor m, Monad m, Applicative m) => PredM m a -> PredM m [a]
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
  :: Functor m
  => PredM m Nothing
  -- ^ What to do on 'Nothing'.  Usually you wil use 'true' or 'false'.
  -> PredM m a
  -- ^ Analyzes 'Just' values.
  -> PredM m (Maybe a)
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
testM :: Functor f => PredM f a -> a -> f Bool
testM (PredM p) = fmap (either (const False) (const True))
  . fmap splitResult . p

-- | Runs a 'Pred' against a value, without a context.
test :: Pred a -> a -> Bool
test p a = runIdentity $ testM p a


-- | Runs a 'Pred' against a particular value; also returns a list of
-- 'Chunk' describing the steps of evaulation.
verboseTestM :: Functor f => PredM f a -> a -> f ([Chunk], Bool)
verboseTestM (PredM f) a = fmap g (f a)
  where
    g rslt = (resultToChunks rslt, resultToBool rslt)

verboseTest :: Pred a -> a -> ([Chunk], Bool)
verboseTest p a = runIdentity $ verboseTestM p a


-- | Obtain a list of 'Chunk' describing the evaluation process.
resultToChunks :: Result -> [Chunk]
resultToChunks = either (failedToChunks 0) (passedToChunks 0)
  . splitResult

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

-- | Like 'verboseTest', but results are printed to standard output.
-- Primarily for use in debugging or in a REPL.
verboseTestStdout :: Pred a -> a -> IO Bool
verboseTestStdout p a = do
  let (cks, r) = verboseTest p a
  t <- smartTermFromEnv IO.stdout
  putChunks t cks
  return r

