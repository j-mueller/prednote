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
-- > tests :: [TestTree Int]
-- > tests = undefined
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
  , seriesAtLeastN

  -- * Grouping tests
  , group

  -- * Running and showing tests
  , Pass
  , Verbosity (..)
  , PassVerbosity
  , FailVerbosity
  , GroupPred
  , TestPred
  , StopOnFail
  , AllPassed
  , EvalEnv (..)
  , evalTree
  , runAllTests
  , runUntilFailure
  , showTestTree
  ) where

import Data.Either (rights)
import Data.Maybe (isJust)
import Data.List (unfoldr)
import Data.Monoid ((<>))
import qualified Data.Text as X
import Data.Text (Text)
import qualified Data.List.Split as Sp

import qualified System.Console.Rainbow as R
import System.Console.Rainbow ((+.+))
import qualified Data.Prednote.Pdct as Pt

--
-- Types
--

type Pass = Bool
type Name = Text

-- | A tree of tests.
data TestTree a = TestTree Name (Payload a)

data Payload a
  = Group [TestTree a]
  | Test (TestFunc a)

type TestFunc a
  = Pt.IndentAmt
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> Pt.Level
  -> (Pass, [R.Chunk])


group :: Name -> [TestTree a] -> TestTree a
group n ts = TestTree n (Group ts)

test :: Name -> TestFunc a -> TestTree a
test n t = TestTree n (Test t)

type PassVerbosity = Verbosity
type FailVerbosity = Verbosity

-- TODO verbosity must be used when showing trees
data Verbosity
  = Silent
  -- ^ Show nothing at all

  | PassFail
  -- ^ Show only whether the test passed or failed

  | FalseSubjects
  -- ^ Show subjects that are False

  | TrueSubjects
  -- ^ Show subjects that are True. (This is cumulative, so False
  -- subjects are shown too.)

  | DiscardedSubjects

  | DiscardedPredicates
  -- ^ Show discarded results
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
  DiscardedSubjects -> (True, False)
  DiscardedPredicates -> (True, True)


showTestTitle :: Pt.IndentAmt -> Pt.Level -> Name -> Pass -> [R.Chunk]
showTestTitle i l n p = [idt, open, passFail, close, blank, txt, nl]
  where
    idt = R.plain (X.replicate (i * l) " ")
    nl = R.plain "\n"
    passFail =
      if p
      then R.plain "PASS" +.+ R.f_green
      else R.plain "FAIL" +.+ R.f_red
    open = R.plain "["
    close = R.plain "]"
    blank = R.plain (X.singleton ' ')
    txt = R.plain n

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
seriesAtLeastN
  :: Name
  -> (a -> X.Text)
  -> Int
  -> Pt.Pdct a
  -> TestTree a
seriesAtLeastN n swr count p = TestTree n (Test tf)
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
indent amt lvl t = R.plain txt
  where
    txt = X.concat [spaces, t, "\n"]
    spaces = X.replicate (amt * lvl) " "

skip :: Text -> Pt.IndentAmt -> Pt.Level -> Text -> [R.Chunk]
skip lbl amt lvl t =
  [ R.plain (X.replicate (amt * lvl) " ")
  , R.plain "["
  , R.plain ("skip " <> lbl) +.+ R.f_yellow
  , R.plain "] "
  , R.plain t
  , R.plain "\n"
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

type GroupPred = Name -> Bool
type TestPred = Name -> Bool
type StopOnFail = Bool

data EvalEnv a = EvalEnv
  { eeIndentAmt :: Pt.IndentAmt
  , eePassVerbosity :: PassVerbosity
  , eeFailVerbosity :: FailVerbosity
  , eeGroupPred :: GroupPred
  , eeTestPred :: TestPred
  , eeSubjects :: [a]
  , eeStopOnFail :: StopOnFail
  }


type ShortCircuit = Bool

evalTree
  :: EvalEnv a
  -> Pt.Level
  -> TestTree a
  -> (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
evalTree ee l (TestTree n p) = case p of
  Group ts -> evalGroup ee n l ts
  Test f -> evalTest ee n l f

evalGroup
  :: EvalEnv a
  -> Name
  -> Pt.Level
  -> [TestTree a]
  -> (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
evalGroup ee n l ts = if eeGroupPred ee n
  then let ls = unfoldr (unfoldList ee l) (False, ts)
           stop = any not . map fst $ ls
           rslts = concat . map snd $ ls
        in (stop, rslts)
  else (False, [Left $ skip "group" (eeIndentAmt ee) l n])

evalTest
  :: EvalEnv a
  -> Name
  -> Pt.Level
  -> TestFunc a
  -> (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
evalTest ee n l tf = if eeTestPred ee n
  then (not p, [Right (p, cs)])
  else (False, [Left $ skip "test" (eeIndentAmt ee) l n])
  where
    (p, cs) = tf (eeIndentAmt ee) (eePassVerbosity ee)
              (eeFailVerbosity ee) (eeSubjects ee) l


--
-- Running a group of tests
--

type NPassed = Int
type NFailed = Int

-- | Runs all tests. Reports on how many passed and how many failed.

runAllTests
  :: Pt.IndentAmt
  -> Pt.Level
  -> GroupPred
  -> TestPred
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> [TestTree a]
  -> ([R.Chunk], NPassed, NFailed)
runAllTests i l gp tp pv fv ss ts =
  let ee = EvalEnv i pv fv gp tp ss False
      rs = concatMap snd . map (evalTree ee l) $ ts
      testRs = rights rs
      passed = length . filter id . map fst $ testRs
      failed = length . filter (not . id) . map fst $ testRs
      cks = concat . map (either id snd) $ rs
  in (cks, passed, failed)


type AllPassed = Bool

-- | Runs tests. Stops running tests if a single test fails. Returns
-- True if all tests passed, or False if a single test failed.
runUntilFailure
  :: Pt.IndentAmt
  -> Pt.Level
  -> GroupPred
  -> TestPred
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> [TestTree a]
  -> ([R.Chunk], AllPassed)
runUntilFailure i l gp tp pv fv ss ts =
  let ee = EvalEnv i pv fv gp tp ss True
      ls = unfoldr (unfoldList ee l) (False, ts)
      allPass = and . map not . map fst $ ls
      cks = concat . map (either id snd) . concatMap snd $ ls
  in (cks, allPass)

unfoldList
  :: EvalEnv a
  -> Pt.Level
  -> (ShortCircuit, [TestTree a])
  -> Maybe ( (ShortCircuit, [Either [R.Chunk] (Pass, [R.Chunk])])
           , (ShortCircuit, [TestTree a]))
unfoldList ee l (seenFalse, is) =
  if seenFalse && eeStopOnFail ee
  then Nothing
  else case is of
        [] -> Nothing
        (TestTree n p):xs ->
          let (short, results) = case p of
                Group ts -> evalGroup ee n (l + 1) ts
                Test tf -> evalTest ee n (l + 1) tf
          in Just ((short, results), (short, xs))


