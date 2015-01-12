-- | Prednote - annotated predicates
--
-- This module exports all the types and functions you will ordinarily
-- need.  Many names clash with Prelude names, because these names
-- made the most sense.  But I didn't make any clashing operators, as
-- I'm not that much of a masochist.  So you will probably want to do
-- something like
--
-- > import qualified Prednote as P
-- > import Prednote ((|||), (&&&))
--
-- For more documentation, first see "Prednote.Core", and then
-- "Prednote.Comparisons" and then "Prednote.Expressions".
module Prednote
  ( module Prednote.Comparisons
  , module Prednote.Expressions
  , module Prednote.Core
  ) where

import Prednote.Comparisons
import Prednote.Expressions
import Prednote.Core
