-- Generates the Cabal file for prednote-test.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import CabalCommon
import qualified Cartel as C

prednote :: C.Package
prednote = C.exactly "prednote" versionInts

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,7,5] [2,8]

tasty :: C.Package
tasty = C.closedOpen "tasty" [0,8,1,1] [0,9]

tasty_quickcheck :: C.Package
tasty_quickcheck = C.closedOpen "tasty-quickcheck" [0,8,1] [0,9]

properties :: C.Properties
properties = commonProperties
  { C.prName = "prednote-test"
  , C.prDescription =
    [ "Tests and QuickCheck instances to accompany prednote." ]
  }

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library
  [ C.LibExposedModules ms
  , C.buildDepends
    [ base
    , rainbow
    , rainbow_tests
    , text
    , prednote
    , quickcheck
    , tasty
    , tasty_quickcheck
    ]
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

cabal
  :: [String]
  -- ^ Modules for library
  -> C.Cabal
cabal ms = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library ms
  }

main :: IO ()
main = do
  mods <- C.modules "lib"
  C.render "genCabal.hs" $ cabal mods
