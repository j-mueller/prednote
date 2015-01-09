-- Generates the Cabal file for prednote.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import CabalCommon
import qualified Cartel as C

-- Dependencies

split :: C.Package
split = C.closedOpen "split" [0,2,2] [0,3]

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,4] [0,5]

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

libDepends :: [C.Package]
libDepends =
  [ base
  , rainbow
  , split
  , text
  , containers
  ]

library
  :: [String]
  -- ^ Library modules
  -> C.Library
library ms = C.Library
  [ C.LibExposedModules ms
  , C.buildDepends libDepends
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

tests
  :: [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> (C.TestSuite, C.Executable)
tests ls ts =
  ( C.TestSuite "prednote-tests" $
    commonTestOpts ls ts ++
    [ C.TestMainIs "prednote-tests.hs"
    , C.TestType C.ExitcodeStdio
    ]
  , C.Executable "prednote-visual-tests" $
    [ C.ExeMainIs "prednote-visual-tests.hs"
    , C.cif (C.flag "visual-tests")
       ( commonTestOpts ls ts ++
        [ C.buildable True
        ]
       )
      [ C.buildable False
      ]
    ]
  )

commonTestOpts
  :: C.Field a
  => [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> [a]
commonTestOpts ls ts =
  [ C.hsSourceDirs ["lib", "tests"]
  , C.otherModules (ls ++ ts)
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  , C.buildDepends $ quickcheck : quickpull : libDepends
  ]

visualTests :: C.Flag
visualTests = C.Flag
  { C.flName = "visual-tests"
  , C.flDescription = "Build the prednote-visual-tests executable"
  , C.flDefault = False
  , C.flManual = True
  }


cabal
  :: [String]
  -- ^ Modules for library
  -> [String]
  -- ^ Modules for tests
  -> C.Cabal
cabal ls ts = C.empty
  { C.cProperties = properties
  , C.cRepositories = [repo]
  , C.cLibrary = Just $ library ls
  , C.cTestSuites = [testSuite]
  , C.cExecutables = [executable]
  , C.cFlags = [visualTests]
  }
  where
    (testSuite, executable) = tests ls ts

main :: IO ()
main = do
  libMods <- C.modules "lib"
  testMods <- C.modules "tests"
  C.render "genCabal.hs" $ cabal libMods testMods
