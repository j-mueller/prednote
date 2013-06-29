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
  -- * The TestTree
    Name
  , TestFunc
  , TestTree (..)
  , Payload (..)
  , test

  -- * Tests
  , eachSubjectMustBeTrue
  , nSubjectsMustBeTrue

  -- * Grouping tests
  , group

  -- * Simple test runners
  , Verbosity(..)
  , GroupVerbosity (..)
  , Pt.Level
  , PassCount
  , FailCount
  , runTests

  -- * Showing the test tree
  , showTestTree

  -- * Tree evaluator
  , TestOpts (..)
  , ShortCircuit
  , Pass
  , evalTree

  ) where

import Data.Either (rights)
import Data.Maybe (isJust)
import Data.List (unfoldr)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as X
import Data.Text (Text)
import qualified Data.List.Split as Sp

import qualified System.Console.Rainbow as R
import qualified Data.Prednote.Pdct as Pt

--
-- Types
--

type Pass = Bool

-- | The name of a test or of a group.
type Name = Text

-- | A tree of tests.
data TestTree a = TestTree Name (Payload a)

data Payload a
  = Group [TestTree a]
  | Test (TestFunc a)

-- | A test is a function of this type. The function must make chunks
-- in a manner that respects the applicable verbosity.
type TestFunc a
  = Pt.IndentAmt
  -> Verbosity
  -- ^ Use this verbosity for tests that pass

  -> Verbosity
  -- ^ Use this verbosity for tests that fail

  -> [a]
  -> Pt.Level
  -> (Pass, [R.Chunk])


-- | Creates groups of tests.
group :: Name -> [TestTree a] -> TestTree a
group n = TestTree n . Group

-- | Creates tests.
test :: Name -> TestFunc a -> TestTree a
test n = TestTree n . Test

-- | How verbose to be when reporting the results of tests. It would
-- be possible to have many more knobs to control this behavior; this
-- implementation is a compromise and hopefully provides enough
-- verbosity settings without being too complex.
data Verbosity

  = Silent
  -- ^ Show nothing at all

  | PassFail
  -- ^ Show only whether the test passed or failed

  | FalseSubjects
  -- ^ Show subjects that are False. In addition, shows all evaluation
  -- steps that led to the subject being False; however, does not show
  -- discarded evaluation steps. Does not show True subjects at all.

  | TrueSubjects
  -- ^ Show subjects that are True. (This is cumulative, so False
  -- subjects are shown too, as they would be using 'FalseSubjects'.)
  -- Shows all evaluation steps that led to the subject being True;
  -- however, does not show discarded evaluation steps.

  | Discards
  -- ^ Shows discarded subjects. Cumulative, so also does what
  -- 'FalseSubjects' and 'TrueSubjects' do. Also shows all discarded
  -- evaluation steps for all subjects.

  deriving (Eq, Ord, Show)

--
-- Helper functions
--


-- | Determines whether to show a subject, and shows it.
showSubject
  :: (a -> X.Text)
  -> Verbosity
  -> Pt.IndentAmt
  -> Pt.Level
  -> Pt.Pdct a
  -> (a, Maybe Bool)
  -> [R.Chunk]
showSubject swr v i l p (s, b) =
  let (showSubj, showDisc) = isSubjectAndDiscardsShown v b
      renamer txt = X.concat [swr s, " - ", txt]
      renamed = Pt.rename renamer p
  in if showSubj
     then snd $ Pt.evaluate i showDisc s l renamed
     else []

-- | Given a Verbosity and a Maybe Boolean indicating whether a
-- subject is True, False, or a discard, returns whether to show the
-- subject and whether to show the discards contained within the
-- subject.
isSubjectAndDiscardsShown :: Verbosity -> Maybe Bool -> (Bool, Bool)
isSubjectAndDiscardsShown v b = case v of
  Silent -> (False, False)
  PassFail -> (False, False)
  FalseSubjects -> (not . isTrue $ b, False)
  TrueSubjects -> (isJust b, False)
  Discards -> (True, True)


-- | Creates a plain Chunk from a Text.
plain :: X.Text -> R.Chunk
plain = R.Chunk mempty

showTestTitle :: Pt.IndentAmt -> Pt.Level -> Name -> Pass -> [R.Chunk]
showTestTitle i l n p = [idt, open, passFail, close, blank, txt, nl]
  where
    idt = plain (X.replicate (i * l) " ")
    nl = plain "\n"
    passFail =
      if p
      then "PASS" <> R.f_green
      else "FAIL" <> R.f_red
    open = plain "["
    close = plain "]"
    blank = plain (X.singleton ' ')
    txt = plain n

isTrue :: Maybe Bool -> Bool
isTrue = maybe False id

--
-- Tests
--

-- | Passes if every subject is True.
eachSubjectMustBeTrue
  :: Name
  -> (a -> Text)
  -> Pt.Pdct a
  -> TestTree a
eachSubjectMustBeTrue n swr p = TestTree n (Test tf)
  where
    tf i pv fv as lvl = (pass, cks)
      where
        rslts = zip as (map (Pt.eval p) as)
        pass = all (isTrue . snd) rslts
        v = if pass then pv else fv
        cks = tit ++ subjectChunks
        tit = if v == Silent then [] else showTestTitle i lvl n pass
        subjectChunks =
          concatMap (showSubject swr v i (lvl + 1) p) rslts

-- | Passes if at least n subjects are True.
nSubjectsMustBeTrue
  :: Name
  -> (a -> X.Text)
  -> Int
  -> Pt.Pdct a
  -> TestTree a
nSubjectsMustBeTrue n swr count p = TestTree n (Test tf)
  where
    tf idnt pv fv as l = (pass, cks)
      where
        pd (_, res) = isTrue res
        resultList = take count
                     . Sp.split ( Sp.keepDelimsR
                                  (Sp.dropFinalBlank . Sp.whenElt $ pd))
                     $ zip as (map (Pt.eval p) as)
        pass = length resultList >= count
        v = if pass then pv else fv
        cks = tit ++ subjectChunks
        tit = if v == Silent then [] else showTestTitle idnt l n pass
        subjectChunks =
          concatMap (showSubject swr v idnt (l + 1) p)
          . concat $ resultList

indent :: Pt.IndentAmt -> Pt.Level -> Text -> R.Chunk
indent amt lvl t = plain txt
  where
    txt = X.concat [spaces, t, "\n"]
    spaces = X.replicate (amt * lvl) " "

skip :: Text -> Pt.IndentAmt -> Pt.Level -> Text -> [R.Chunk]
skip lbl amt lvl t =
  [ plain (X.replicate (amt * lvl) " ")
  , plain "["
  , plain ("skip " <> lbl) <> R.f_yellow
  , plain "] "
  , plain t
  , plain "\n"
  ]

-- | Shows a tree, without evaluating it.
showTestTree
  :: Pt.IndentAmt
  -> Pt.Level
  -> TestTree a
  -> [R.Chunk]
showTestTree amt l (TestTree n p) = indent amt l n : children
  where
    children = case p of
      Group ts -> concatMap (showTestTree amt l) ts
      Test _ -> []


-- | Options for running tests.
data TestOpts a = TestOpts

  { tIndentAmt :: Int
    -- ^ Indent each level by this many spaces

  , tPassVerbosity :: Verbosity
    -- ^ Use this verbosity for tests that pass

  , tFailVerbosity :: Verbosity
    -- ^ Use this verbosity for tests that fail

  , tGroupPred :: Name -> Bool
    -- ^ Groups are run only if this predicate returns True.

  , tTestPred :: Name -> Bool
    -- ^ Tests are run only if this predicate returns True.

  , tShowSkippedTests :: Bool
    -- ^ Some tests might be skipped; see 'tTestPred'. This controls
    -- whether you want to see a notification of tests that were
    -- skipped. (Does not affect skipped groups; see 'tGroupVerbosity'
    -- for that.)

  , tGroupVerbosity :: GroupVerbosity
    -- ^ Show group names? Even if you do not show the names of
    -- groups, tests within the group will still be indented.

  , tSubjects :: [a]
    -- ^ The subjects to test

  , tStopOnFail :: Bool
    -- ^ If True, then tests will stop running immediately after a
    -- single test fails. If False, all tests are always run.
  }


-- | True if the tree returned a result without completely evaluating
-- all parts of the tree. This can occur if 'tStopOnFail' is True and
-- one of the tests in the tree failed.
type ShortCircuit = Bool

-- | Evaluates a tree. This function is the basis of 'runTests', which
-- is typically a bit easier to use.
evalTree

  :: TestOpts a
  -- ^ Most options

  -> Pt.Level
  -- ^ The tree will indented by this many levels; typically you will
  -- want to start this at 0.

  -> TestTree a

  -> (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
  -- ^ The first element of the tuple is True if the tree was not
  -- fully evaluated. This can happen if 'tStopOnFail' is True and
  -- one of the tests in the tree failed. The second element of the
  -- tuple is a list of Either; each element of the list will be a
  -- Left if that component of the tree was not a test, or a Right if
  -- that element was a test. The Right will contain a tuple, where
  -- the first element indicates whether the test passed or failed,
  -- and the second element is the list of Chunk.

evalTree ee l (TestTree n p) = case p of
  Group ts -> evalGroup ee n l ts
  Test f -> evalTest ee n l f

evalGroup
  :: TestOpts a
  -> Name
  -> Pt.Level
  -> [TestTree a]
  -> (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
evalGroup ee n l ts = if tGroupPred ee n
  then let ls = unfoldr (unfoldList ee l) (False, ts)
           stop = any not . map fst $ ls
           rslts = concat . map snd $ ls
           groupNm = if tGroupVerbosity ee /= NoGroups
                     then indent (tIndentAmt ee) l n
                     else plain ""
        in (stop, Left [groupNm] : rslts)
  else let groupNm = if tGroupVerbosity ee == AllGroups
                     then skip "group" (tIndentAmt ee) l n
                     else [plain ""]
       in (False, [Left groupNm])


evalTest
  :: TestOpts a
  -> Name
  -> Pt.Level
  -> TestFunc a
  -> (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
evalTest ee n l tf = if tTestPred ee n
  then (not p, [Right (p, cs)])
  else (False, skipped)
  where
    (p, cs) = tf (tIndentAmt ee) (tPassVerbosity ee)
              (tFailVerbosity ee) (tSubjects ee) l
    skipped = if tShowSkippedTests ee
              then [Left $ skip "test" (tIndentAmt ee) l n]
              else []


--
-- Running a group of tests
--

type PassCount = Int
type FailCount = Int

-- | How verbose to be when showing names of groups.
data GroupVerbosity

  = NoGroups
  -- ^ Show no group names at all. However, groups will still be
  -- indented.

  | ActiveGroups
  -- ^ Show groups that are not skipped.

  | AllGroups
  -- ^ Show all groups, and indicate which groups are skipped.
  deriving (Eq, Ord, Show)


-- | Runs each test in a list of tests (though each test might not run
-- if 'tStopOnFail' is True.) Reports on how many passed and how many
-- failed. (if 'tStopOnFail' is True, the FailCount will never exceed
-- 1.)
runTests
  :: TestOpts a
  -> Pt.Level
  -> [TestTree a]
  -> ([R.Chunk], PassCount, FailCount)
runTests ee l ts =
  let ls = unfoldr (unfoldList ee l) (False, ts)
      testRs = rights . concatMap snd $ ls
      passed = length . filter id . map fst $ testRs
      failed = length . filter (not . id) . map fst $ testRs
      cks = concat . map (either id snd) . concatMap snd $ ls
  in (cks, passed, failed)

unfoldList
  :: TestOpts a
  -> Pt.Level
  -> (ShortCircuit, [TestTree a])
  -> Maybe ( (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
           , (ShortCircuit, [TestTree a]))
unfoldList ee l (seenFalse, is) =
  if seenFalse && tStopOnFail ee
  then Nothing
  else case is of
        [] -> Nothing
        t:xs ->
          let (short, results) = evalTree ee l t
          in Just ((short, results), (short, xs))


