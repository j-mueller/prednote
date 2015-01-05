-- Generates the Cabal file for prednote-test.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main (main) where

import CabalCommon
import qualified Cartel as C

prednote :: C.Package
prednote = C.exactly "prednote" versionInts

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,7,5] [2,8]

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,4] [0,5]

properties :: C.Properties
properties = commonProperties
  { C.prName = "prednote-test"
  , C.prSynopsis = "Tests and QuickCheck generators to accompany prednote."
  , C.prDescription =
    [ "These are packaged separately so other packages may depend"
    , "on them."
    ]
  , C.prExtraSourceFiles =
    [ "genCabal.hs"
    ]
  }

depends :: [C.Package]
depends =
  [ base
  , rainbow
  , rainbow_tests
  , text
  , prednote
  , quickcheck
  , containers
  , quickpull
  , barecheck
  ]

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library
  [ C.LibExposedModules ms
  , C.buildDepends depends
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

executable :: [String] -> C.Executable
executable ms = C.Executable "prednote-test"
  [ C.hsSourceDirs ["lib", "exe"]
  , C.otherModules ms
  , C.ExeMainIs "prednote-test.hs"
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  , C.buildDepends depends
  ]

{-
visual :: C.Executable
visual = C.Executable "prednote-visual-test"
  [ C.hsSourceDirs ["lib", "visual"]
  , C.ExeMainIs "prednote-visual-test.hs"
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  , C.buildDepends depends
  ]
-}
cabal
  :: [String]
  -- ^ Modules for library
  -> [String]
  -- ^ Modules for the executable
  -> C.Cabal
cabal msLib msExe = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library msLib
  , C.cExecutables = [executable (msLib ++ msExe)]
  -- , C.cExecutables = [executable (msLib ++ msExe), visual]
  }

main :: IO ()
main = do
  mods <- C.modules "lib"
  modsExe <- C.modules "exe"
  C.render "genCabal.hs" $ cabal mods modsExe
