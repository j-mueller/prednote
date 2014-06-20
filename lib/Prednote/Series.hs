{-# LANGUAGE OverloadedStrings #-}

-- | Helps you build tests that run against a series of items.
module Prednote.Series where
{-
  (
  -- * Test data types
    Name
  , Verbosity(..)
  , TrueVerbosity
  , FalseVerbosity
  , TestVisibility(..)
  , TestVerbosity(..)
  , Pass
  , Test(..)
  , TestResult(..)

  -- * Pre-built tests
  , eachSubjectMustBeTrue
  , nSubjectsMustBeTrue

  -- * Running and showing tests
  , evalTest
  , showTestResult

  ) where
-}

import Control.Arrow (first)
import Data.Functor.Contravariant
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as X
import Data.Text (Text)

import qualified System.Console.Rainbow as R
import qualified Prednote.Predbox.Core as Pt

-- # Types

-- | How verbose to be when showing the results of running a Predbox on a
-- single subject.
data Verbosity
  = HideAll
  -- ^ Do not show any results from the Predbox

  | ShowDefaults
  -- ^ Show results according to the default settings provided in the
  -- Result itself

  | ShowAll
  -- ^ Show all Result
  deriving (Eq, Show)

-- | Determines whether to show any of the results from a single series.
data Visibility
  = HideSeries
  -- ^ Do not show any results from this series

  | ShowFirstLine Verbosity Verbosity
  -- ^ Show the first line, which indicates whether the series passed or
  -- failed and gives the label for the test. Whether to show
  -- individual subjects is determined by the first Verbosity (which
  -- is used for subjects that are True) and the second Verbosity
  -- (used for subjects that are False).

  deriving (Eq, Show)

-- | Determines which Visibility to use for a particular Series.
data VisibilitySet = VisibilitySet
  { onPass :: Visibility
    -- ^ Use this Visibility when the series passes
  , onFail :: Visibility
    -- ^ Use this Visibility when the series fails
  } deriving (Eq, Show)

-- | A single series.
data Series a = Series
  { sName :: Text
  , sPass :: [Pt.Result] -> Bool
  -- ^ Applied to the results of all applications of testFunc;
  -- determines whether the Series passes or fails.

  , sFunc :: a -> Pt.Result
  -- ^ This function is applied to each subject.

  , sVisibility :: VisibilitySet
  -- ^ Visibility for the series
  }

instance Contravariant Series where
  contramap f t = t { sFunc = sFunc t . f }

data SeriesResult a = SeriesResult
  { resultName :: Text
  , resultPass :: Bool
  , resultSubjects :: [(a, Pt.Result)]
  , resultVisibility :: VisibilitySet
  }

instance Functor SeriesResult where
  fmap f t = t { resultSubjects = map (first f) . resultSubjects $ t }

-- | Evaluates a Series for a given list of subjects.
evalSeries :: Series a -> [a] -> SeriesResult a
evalSeries (Series n fPass fSubj vy) ls = SeriesResult n p ss vy
  where
    p = fPass results
    results = map fSubj ls
    ss = zip ls results

-- | The Series passes if each subject returns True.
eachSubjectMustBeTrue :: Pt.Predbox a -> Text -> Series a
eachSubjectMustBeTrue pd nm = Series nm pass f vy
  where
    vy = VisibilitySet
      { onPass = ShowFirstLine HideAll HideAll
      , onFail = ShowFirstLine HideAll ShowDefaults }
    pass = all Pt.rBool
    f = Pt.evaluate pd


-- | The test passes if at least a given number of subjects are True.
nSubjectsMustBeTrue
  :: Pt.Predbox a
  -> Text
  -> Int
  -- ^ The number of subjects that must be True. This should be a
  -- positive number.
  -> Series a
nSubjectsMustBeTrue pd nm i = Series nm pass f vy
  where
    pass = atLeast i . filter Pt.rBool
    f = Pt.evaluate pd
    vy = VisibilitySet
      { onPass = ShowFirstLine HideAll HideAll
      , onFail = ShowFirstLine HideAll HideAll }


-- | Returns True if the list has at least this many elements. Lazier
-- than taking the length of the list.
atLeast :: Int -> [a] -> Bool
atLeast i as
  | i < 0 = error "atLeast: negative length parameter"
  | otherwise = go 0 as
  where
    go _ [] = i == 0
    go soFar (_:xs) =
      let nFound = soFar + 1
      in if nFound == i then True else go nFound xs

{-

-- # Showing tests

{-
showTestTitle :: Name -> Pass -> [R.Chunk]
showTestTitle n p = [open, passFail, close, blank, txt, nl]
  where
    nl = plain "\n"
    passFail =
      if p
      then "PASS" <> R.f_green
      else "FAIL" <> R.f_red
    open = plain "["
    close = plain "]"
    blank = plain (X.singleton ' ')
    txt = plain n
-}
-- | Shows a result with indenting.
showTestResult
  :: Pt.IndentAmt
  -- ^ Indent each level by this many spaces

  -> (a -> Text)
  -- ^ Shows each subject. The function should return a single-line
  -- text without a trailing newline.

  -> Maybe TestVerbosity
  -- ^ If Just, use this TestVerbosity when showing the test. If
  -- Nothing, use the default verbosity.

  -> TestResult a
  -- ^ The result to show

  -> [R.Chunk]
showTestResult amt swr mayVb (TestResult n p ss dfltVb) =
  let vb = fromMaybe dfltVb mayVb
      tv = if p then onPass vb else onFail vb
      firstLine = showTestTitle n p
  in case tv of
      HideTest -> []
      ShowFirstLine trueV falseV ->
        firstLine
        ++ concatMap (showSubject p amt swr (trueV, falseV)) ss

showSubject
  :: Pass
  -> Pt.IndentAmt
  -> (a -> Text)
  -> (TrueVerbosity, FalseVerbosity)
  -> (a, Pt.Result)
  -> [R.Chunk]
showSubject p amt swr (tv, fv) (a, r) =
  let txt = swr a
      vb = if p then tv else fv
  in case vb of
      HideAll -> []
      ShowDefaults -> Pt.showTopResult txt amt 1 False r
      ShowAll -> Pt.showTopResult txt amt 1 True r

-- # Pre-built tests



-- # Basement


-}
