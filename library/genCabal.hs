-- Generates the Cabal file for prednote.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import CabalCommon
import qualified Cartel as C

-- Dependencies

contravariant :: C.Package
contravariant = C.closedOpen "contravariant" [0,2,0,1] [0,7]

split :: C.Package
split = C.closedOpen "split" [0,2,2] [0,3]

properties :: C.Properties
properties = commonProperties
  { C.prName = "prednote"
  , C.prDescription =
    [ "Build and evaluate trees of predicates. For example, you might build"
    , "a predicate of the type Int -> Bool. You do this by assembling"
    , "several predicates into a tree. You can then verbosely evaluate"
    , "this tree, showing why a particular result is reached."
    , ""
    , "prednote also provides modules to test several subjects against a"
    , "given predicate, and to parse infix or RPN expressions into a tree of"
    , "predicates."
    ]
  , C.prTestedWith = map (\ls -> (C.GHC, C.eq ls))
    [ [7,4,1], [7,6,3], [7,8,2] ]
  , C.prExtraSourceFiles =
    [ "README.md"
    , "minimum-versions.txt"
    , "current-versions.txt"
    , "sunlight-test.hs"
    , "changelog"
    ]
  }

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library
  [ C.LibExposedModules ms
  , C.buildDepends
    [ base
    , contravariant
    , rainbow
    , split
    , text
    , containers
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
