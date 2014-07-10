-- | This module provides everything you need for most uses of Prednote.
-- The core type of Prednote is the 'Pred', which is a rose
-- 'Tree' of predicates along with some additional information, such
-- as a 'plan', which shows you how the 'Pred' will be evaluated
-- without actually having to apply it to a particular subject.  When
-- evaluating a 'Pred', you can also display a 'report' describing the
-- evaluation process.
--
-- This module builds 'Pred' with 'report's that make sparing use of
-- color; for example, 'True' results have @[TRUE]@ indicated in
-- green, @[FALSE]@ in red, and /short circuits/ (that is, 'Pred' that
-- were evaluated without evaluating all their child 'Pred') indicated
-- in yellow.  They are also nicely indented to indicate the structure
-- of the 'Tree'.
--
-- If you want more control over how your results are formatted,
-- examine "Prednote.Core" and "Prednote.Format".
-- "Prednote.Comparisons" builds on this module to provide 'Pred' to
-- use for common comparisons (such as greater than, less than, etc.)
-- "Prednote.Expressions" helps you parse infix or postfix (i.e. RPN)
-- expressions.
--
-- This module exports some names that conflict with Prelude names, so
-- you might want to do something like
--
-- > import qualified Prednote as P
module Prednote
  ( -- * Pred
    Pred

    -- * Visibility

    -- | Upon evaluation, each 'Pred' has a visibility, indicated with
    -- 'Visible'.  It can be either 'shown' or 'hidden'.  The
    -- visibility of a 'Pred' does not affect any of the results, nor
    -- does it affect how the 'Pred' is shown in the 'plan'; rather,
    -- it affects only how the result of the 'Pred' is shown in the
    -- 'report'.  If a 'Pred' is 'hidden', its value and the value of
    -- its children is not shown in the 'report'.

  , C.Visible
  , C.shown
  , C.hidden
  , P.visibility
  , P.reveal
  , P.hide
  , P.showTrue
  , P.showFalse

    -- * Predicates

    -- | These 'Pred' have no child 'Pred'.

  , P.predicate
  , P.true
  , P.false
  , P.same

  -- * Combinators

  -- | These functions combine one more more 'Pred' to create a new
  -- 'Pred'; the argument 'Pred' become children of the new 'Pred'.

  , P.all
  , (&&&)
  , P.any
  , (|||)
  , P.not

  -- ** Fanout

  -- | These functions allow you to take a single subject and split it
  -- into multiple subjects, applying a 'Pred' to each subject that
  -- results.  As a simple example, this allows you to build a 'Pred'
  -- ['Int'] that combines 'Pred' that test individual 'Int' along
  -- with 'Pred' that examine the entire list of ['Int'].

  , P.fanAll
  , P.fanAny
  , P.fanAtLeast

  -- * Reports and plans

  -- | A 'plan' displays a 'Pred' without evaluating it, while a
  -- 'report' shows the process through which a 'Pred' was evaluated
  -- for a particular subject.

  , C.Output
  , plan
  , C.evaluate
  , report

  -- * Evaluation and reporting

  -- | These functions use 'report', 'C.evaluate', or both.

  , C.test
  , C.testV
  , C.filter
  , C.filterV
  ) where

import qualified Prednote.Prebuilt as P
import Prednote.Prebuilt ((&&&), (|||))
import Prednote.Core (Pred)
import qualified Prednote.Core as C
import System.Console.Rainbow
import Data.Tree

-- | Indents and formats static labels for display.  This is a 'plan'
-- for how the 'Pred' would be applied.
plan :: Pred a -> [Chunk]
plan = C.plan 0

-- | Indents and formats output for display.
report :: Tree C.Output -> [Chunk]
report = C.report 0
