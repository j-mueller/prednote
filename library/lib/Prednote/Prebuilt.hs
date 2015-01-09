{-# LANGUAGE NoImplicitPrelude #-}
module Prednote.Prebuilt
  ( -- * Predicates
    Pdct
    -- * Simple predicates
  , true
  , false
  , same

    -- * Describing types
  , Typedesc(..)
  , Typeshow(..)
  , typeshow
  , renderTypedesc
  , renderInnerTypedesc

    -- * Creating predicates
  , predicate

    -- * Predicate combinators - boolean
  , (&&&)
  , (|||)
  , not

    -- * Predicate combinators - sum types
  , either
  , maybe

    -- * Predicate combinators - lists
  , any
  , all

    -- * Wrapping
  , wrap

    -- * Evaluating predicates
  , test
  ) where

import Prednote.Prebuilt.Internal
