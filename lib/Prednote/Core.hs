{-# LANGUAGE OverloadedStrings #-}
module Prednote.Core
  ( -- * Predicates and their creation
    PredM(..)
  , Pred
  , predicate
  , predicateM
  , contramapM

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
import Data.List (intersperse)
import Data.Functor.Identity
import Control.Applicative

-- | Like 'contramap' but allows the mapping function to run in a
-- monad.
contramapM
  :: Monad m
  => (a -> m b)
  -> PredM m b
  -> PredM m a
contramapM conv (PredM f) = PredM $ \a -> conv a >>= f

-- | Describes the condition; for example, for a @'Pred' 'Int'@,
-- this might be @is greater than 5@; for a @'Pred' 'String'@, this
-- might be @begins with \"Hello\"@.
newtype Condition = Condition [Chunk]
  deriving (Eq, Ord, Show)

instance Monoid Condition where
  mempty = Condition []
  mappend (Condition x) (Condition y) = Condition (x ++ y)

-- | Stores the representation of a value.
newtype Value = Value [Chunk]
  deriving (Eq, Ord, Show)

instance Monoid Value where
  mempty = Value []
  mappend (Value x) (Value y) = Value (x ++ y)

-- | Gives additional information about a particular 'Pred' to aid the
-- user when viewing the output.
newtype Label = Label [Chunk]
  deriving (Eq, Ord, Show)

instance Monoid Label where
  mempty = Label []
  mappend (Label x) (Label y) = Label (x ++ y)

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
  :: Functor f
  => (a -> f (Bool, Value, Condition))
  -> PredM f a
predicateM f = PredM f'
  where
    f' a = fmap mkResult $ f a
      where
        mkResult (b, val, cond) = Result (Labeled [] r)
          where
            r | b = Right (PTerminal val cond)
              | otherwise = Left (FTerminal val cond)

-- | Creates a new 'Pred' that do not run in any context.  In
-- @predicate cond f@, @cond@ describes the condition, while @f@ gives
-- the predicate function.  For example, if @f@ is @(> 5)@, then
-- @cond@ might be @"is greater than 5"@.
predicate
  :: (a -> (Bool, Value, Condition))
  -> Pred a
predicate f = predicateM (fmap return f)

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
true :: Applicative f => PredM f a
true = predicateM (const (pure trip))
  where
    trip = (True, mempty, Condition ["always returns True"])

-- | Always returns 'False'
false :: Applicative f => PredM f a
false = predicateM (const (pure trip))
  where
    trip = (False, mempty, Condition ["always returns False"])

-- | Always returns its argument
same :: Applicative f => PredM f Bool
same = predicateM
  (\b -> pure (b, (Value [(fromText . X.pack . show $ b)]),
                  Condition ["is returned"]))

-- | Adds descriptive text to a 'Pred'.  Gives useful information for
-- the user.  The label is added to the top 'Pred' in the tree; any
-- existing labels are also retained.  Labels that were added last
-- will be printed first.  For an example of this, see the source code
-- for 'any' and 'all'.
addLabel :: Functor f => [Chunk] -> PredM f a -> PredM f a
addLabel s (PredM f) = PredM f'
  where
    f' a = fmap g (f a)
      where
        g (Result (Labeled ss ei)) = Result (Labeled (Label s : ss) ei)


-- | Like 'Prelude.any'; is 'True' if any of the list items are
-- 'True'.  An empty list returns 'False'.  Is lazy; will stop
-- processing if it encounters a 'True' item.
any :: (Monad m, Applicative m) => PredM m a -> PredM m [a]
any pa = contramap f (switch (addLabel ["cons cell"] pConsCell) pEnd)
  where
    pConsCell =
      contramap fst (addLabel ["head"] pa)
      ||| contramap snd (addLabel ["tail"] (any pa))
    f ls = case ls of
      [] -> Right ()
      x:xs -> Left (x, xs)
    pEnd = predicateM (const (pure (False, Value ["end of list"],
                                    Condition ["always returns False"])))

-- | Like 'Prelude.all'; is 'True' if none of the list items is
-- 'False'.  An empty list returns 'True'.  Is lazy; will stop
-- processing if it encouters a 'False' item.
all :: (Monad m, Applicative m) => PredM m a -> PredM m [a]
all pa = contramap f (switch (addLabel ["cons cell"] pConsCell) pEnd)
  where
    pConsCell =
      contramap fst (addLabel ["head"] pa)
      &&& contramap snd (addLabel ["tail"] (all pa))
    f ls = case ls of
      x:xs -> Left (x, xs)
      [] -> Right ()
    pEnd = predicateM (const (pure (True, Value ["end of list"],
                                    Condition ["always returns True"])))


-- | Create a 'Pred' for 'Maybe'.
maybe
  :: Applicative m
  => Bool
  -- ^ What to return on 'Nothing'
  -> PredM m a
  -- ^ Analyzes 'Just' values
  -> PredM m (Maybe a)
maybe onEmp pa = contramap f
  (switch emp (addLabel ["Just value"] pa))
  where
    emp | onEmp = predicateM (const
            (pure (True, noth, Condition ["always returns True"])))
        | otherwise = predicateM (const
            (pure (False, noth, Condition ["always returns False"])))
    noth = Value ["Nothing"]
    f may = case may of
      Nothing -> Left ()
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
labelToChunks (Label cks) = cks

explainTerminal :: Value -> Condition -> [Chunk]
explainTerminal (Value v) (Condition c)
  = v ++ (" " : c)

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

