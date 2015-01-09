-- Generates the Cabal file for prednote.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import CabalCommon
import qualified Cartel as C

-- Dependencies

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
    , ""
    , "tests are packaged separately in the prednote-tests package."
    ]
  , C.prTestedWith = map (\ls -> (C.GHC, C.eq ls))
    [ [7,6,3], [7,8,2] ]
  , C.prExtraSourceFiles =
    [ "README.md"
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
    , rainbow
    , split
    , text
    , containers
    , quickcheck
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
