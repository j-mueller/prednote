{-# LANGUAGE NoImplicitPrelude #-}
module Prednote.Prebuilt
  ( -- * Predicates
    C.Pred
    -- * Simple predicates
  , true
  , false
  , same

    -- * Describing types
  , Typedesc(..)
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
  , eitherShower
  , maybeShower

    -- * Predicate combinators - lists
  , any
  , all
  , anyShower
  , allShower

    -- * Wrapping
  , wrap

    -- * Evaluating predicates
  , C.test
  ) where

import Prednote.Prebuilt.Internal
import qualified Prednote.Core as C
