{-# LANGUAGE OverloadedStrings, BangPatterns #-}

-- | Functions to work with 'Pred'.  This module works with 'Text' and
-- produces 'Pred' that make sparing use of color.  For more control
-- over the 'Pred' produced, use "Prednote.Pred.Core".
--
-- Exports some names that conflict with Prelude names, such as 'and',
-- 'or', 'not', and 'filter'; keep this in mind when you do your
-- @import@.
module Prednote.Pred where
