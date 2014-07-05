-- Generates the Cabal file for prednote-test.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import CabalCommon
import qualified Cartel as C

prednote :: C.Package
prednote = C.exactly "prednote" versionInts

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,7,2] [2,8]

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,2,0,0] [0,3]

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
    , containers
    ]
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

executable :: C.Executable
executable = C.Executable "prednote-test"
  [ C.hsSourceDirs ["lib", "exe"]
  , C.ExeMainIs "prednote-test.hs"
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  , C.buildDepends
    [ base
    , rainbow
    , rainbow_tests
    , text
    , prednote
    , quickcheck
    , quickpull
    ]
  ]

cabal
  :: [String]
  -- ^ Modules for library
  -> C.Cabal
cabal ms = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library ms
  , C.cExecutables = [executable]
  }

main :: IO ()
main = do
  mods <- C.modules "lib"
  C.render "genCabal.hs" $ cabal mods
