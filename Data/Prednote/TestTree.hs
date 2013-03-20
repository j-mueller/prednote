{-# LANGUAGE OverloadedStrings #-}

module Data.Prednote.TestTree where

import Data.Maybe (isJust)
import qualified Data.Text as X
import Data.Text (Text)
import qualified Data.List.Split as Sp

import qualified System.Console.Rainbow as R
import qualified Data.Prednote.Pdct as Pt

--
-- Types
--

type Pass = Bool
type Name = Text

-- | A tree of tests. On evaluation of the tree, the name is not shown
-- for tests (it is only shown for groups.) However, the name is used
-- when the tree is displayed statically, without evaluation.
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
  -> (Pass, [C.Chunk])


group :: Name -> [TestTree a] -> TestTree a
group n ts = TestTree n (Group ts)

test :: Name -> TestFunc a -> TestTree a
test n t = TestTree n (Test t)

type PassVerbosity = Verbosity
type FailVerbosity = Verbosity

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
    passFail = C.chunk ts tf
    idt = C.chunk C.defaultTextSpec (X.replicate (i * l) " ")
    nl = C.chunk C.defaultTextSpec "\n"
    (tf, ts) =
      if p
      then ("PASS", Sw.switchForeground C.color8_f_green
                    C.color256_f_2 C.defaultTextSpec)
      else ("FAIL", Sw.switchForeground C.color8_f_red
                    C.color256_f_1 C.defaultTextSpec)
    open = C.chunk C.defaultTextSpec "["
    close = C.chunk C.defaultTextSpec "]"
    blank = C.chunk C.defaultTextSpec (X.singleton ' ')
    txt = C.chunk C.defaultTextSpec n

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
          concatMap (showSubject swr v idnt (l + 1) p) . concat $ resultList

indent :: Pt.IndentAmt -> Pt.Level -> Text -> R.Chunk
indent amt lvl t = C.chunk ts txt
  where
    ts = C.defaultTextSpec
    txt = X.concat [spaces, t, "\n"]
    spaces = X.replicate (amt * lvl) " "

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

evalTestTree
  :: Pt.IndentAmt
  -> Pt.Level
  -> PassVerbosity
  -> FailVerbosity
  -> [a]
  -> TestTree a
  -> [Either R.Chunk (Pass, [R.Chunk])]
evalTestTree i l pv fv as (TestTree n p) = case p of
  Test f -> [Right $ f i pv fv as l]
  Group ts -> Left (indent i l n)
              : concatMap (evalTestTree i (l + 1) pv fv as) ts
