{-# LANGUAGE OverloadedStrings #-}

-- | Helps you build a tree of tests that run against a series of
-- items. This is best illustrated with an example.
--
-- Let's say that you have a list of Int. You want to make sure that
-- every Int in the list is odd and that every Int is greater than
-- zero. You also want to make sure that at least 5 Ints in the list
-- are greater than 20.
--
-- 'Pdct' from "Data.Prednote.Pdct" will help you, but only so much: a
-- 'Pdct' can test individual Int, but by itself it will not help you
-- run a check against a whole list of Int. Of course you can build
-- such a test fairly easily with 'any' and 'all', but what if you
-- want to view the results of the tests verbosely? That's where this
-- module comes in.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import System.Console.Rainbow
-- > import Data.Prednote.TestTree
-- > import Data.Prednote.Pdct
-- >
-- > isOdd :: Pdct Int
-- > isOdd = operand "is odd" odd
-- >
-- > greaterThan0 :: Pdct Int
-- > greaterThan0 = operand "greater than zero" (> 0)
-- >
-- > greaterThan20 :: Pdct Int
-- > greaterThan20 = operand "greater than 20" (> 20)
-- >
-- > myOpts :: TestOpts Int
-- > myOpts = TestOpts
-- >   { tIndentAmt = 2
-- >   , tPassVerbosity = TrueSubjects
-- >   , tFailVerbosity = TrueSubjects
-- >   , tGroupPred = const True
-- >   , tTestPred = const True
-- >   , tShowSkippedTests = True
-- >   , tGroupVerbosity = AllGroups
-- >   , tSubjects = mySubjects
-- >   , tStopOnFail = False
-- >   }
-- >
-- > mySubjects :: [Int]
-- > mySubjects = [2, 4, 6, 8, 10, 18, 19, 20, 21, 22, 24, 26]
-- >
-- > tests :: [TestTree Int]
-- > tests = [ isOdd, greaterThan0, greaterThan20 ]
-- >
-- > main :: IO ()
-- > main = do
-- >   let (cks, passed, failed) = runTests myOpts 0 tests
-- >   t <- termFromEnv
-- >   printChunks t cks
-- >   putStrLn $ "number of tests passed: " ++ show passed
-- >   putStrLn $ "number of tests failed: " ++ show failed
module Data.Prednote.TestTree
  (
  -- * Test data types
    Name
  , Verbosity(..)
  , TrueVerbosity
  , FalseVerbosity
  , ShowTest(..)
  , TestVerbosity
  , Pass
  , TestFunc
  , Test(..)
  , TestResult(..)

  -- * Pre-built tests
  , eachSubjectMustBeTrue
  , nSubjectsMustBeTrue

  -- * Running and showing tests
  , evalTest
  , showResult

  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as X
import Data.Text (Text)

import qualified System.Console.Rainbow as R
import qualified Data.Prednote.Pdct as Pt

-- # Types

data Verbosity
  = HideAll
  | ShowDefaults
  | ShowAll
  deriving (Eq, Show)

type TrueVerbosity = Verbosity
type FalseVerbosity = Verbosity

data ShowTest
  = HideTest
  | ShowFirstLine TrueVerbosity FalseVerbosity
  deriving (Eq, Show)

data TestVerbosity = TestVerbosity
  { onPass :: ShowTest
  , onFail :: ShowTest
  } deriving (Eq, Show)

type Pass = Bool

-- | The name of a test or of a group.
type Name = Text

-- | A single test.
data Test a = Test
  { testName :: Name
  , testFunc :: TestFunc a
  }

data TestResult a = TestResult
  { resultName :: Name
  , resultPass :: Pass
  , resultSubjects :: [(a, Pt.Result)]
  , resultDefaultVerbosity :: TestVerbosity
  }

-- | A test is a function of this type. The function must make chunks
-- in a manner that respects the applicable verbosity.
type TestFunc a = [a] -> (Pass, [(a, Pt.Result)], TestVerbosity)

-- # Showing tests

-- | Creates a plain Chunk from a Text.
plain :: X.Text -> R.Chunk
plain = R.Chunk mempty

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

evalTest :: Test a -> [a] -> TestResult a
evalTest (Test n f) ls = TestResult n p ss v
  where
    (p, ss, v) = f ls

showResult
  :: Pt.IndentAmt
  -> (a -> Text)
  -> Maybe TestVerbosity
  -> TestResult a
  -> [R.Chunk]
showResult amt swr mayVb (TestResult n p ss dfltVb) =
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
      ShowDefaults -> Pt.showTopResult txt amt False r
      ShowAll -> Pt.showTopResult txt amt True r

-- # Pre-built tests

eachSubjectMustBeTrue :: Pt.Pdct a -> TestFunc a
eachSubjectMustBeTrue pd ls = (pass, subjs, vy)
  where
    vy = TestVerbosity
      { onPass = ShowFirstLine HideAll HideAll
      , onFail = ShowFirstLine HideAll ShowDefaults }
    rs = map (flip Pt.evaluate pd) ls
    subjs = zip ls rs
    pass = all Pt.rBool rs


nSubjectsMustBeTrue :: Pt.Pdct a -> Int -> TestFunc a
nSubjectsMustBeTrue pd i ls = (pass, subjs, vy)
  where
    pass = atLeast i . filter Pt.rBool $ rs
    rs = map (flip Pt.evaluate pd) ls
    subjs = zip ls rs
    vy = TestVerbosity
      { onPass = ShowFirstLine HideAll HideAll
      , onFail = ShowFirstLine HideAll HideAll }


-- # Basement

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

