-- Generates the Cabal file for prednote.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import qualified Cartel as C

versionInts :: [Int]
versionInts = [0,28,0,0]

base :: C.Package
base = C.closedOpen "base" [4,5,0,0] [5]

rainbowLower :: [Int]
rainbowLower = [0,20,0,4]

rainbowUpper :: [Int]
rainbowUpper = [0,21]

rainbow :: C.Package
rainbow = C.closedOpen "rainbow" rainbowLower rainbowUpper

rainbow_tests :: C.Package
rainbow_tests = C.closedOpen "rainbow-tests" rainbowLower rainbowUpper

text :: C.Package
text = C.closedOpen "text" [0,11,2,0] [1,3]

containers :: C.Package
containers = C.closedOpen "containers" [0,4,2,1] [0,6]

barecheck :: C.Package
barecheck = C.closedOpen "barecheck" [0,2,0,0] [0,3]

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,5] [2,8]

commonProperties :: C.Properties
commonProperties = C.empty
  { C.prVersion = C.Version versionInts
  , C.prLicenseFile = "LICENSE"
  , C.prCopyright = "Copyright 2013-2015 Omari Norman"
  , C.prAuthor = "Omari Norman"
  , C.prMaintainer = "omari@smileystation.com"
  , C.prStability = "Experimental"
  , C.prHomepage = "http://www.github.com/massysett/prednote"
  , C.prBugReports = "http://www.github.com/massysett/prednote/issues"
  , C.prCategory = "Data"
  , C.prSynopsis = "Evaluate and display trees of predicates"
  }

repo :: C.Repository
repo = C.empty
  { C.repoVcs = C.Git
  , C.repoKind = C.Head
  , C.repoLocation = "git://github.com/massysett/prednote.git"
  , C.repoBranch = "master"
  }

ghcOptions :: [String]
ghcOptions = ["-Wall"]

-- Dependencies

split :: C.Package
split = C.closedOpen "split" [0,2,2] [0,3]

quickpull :: C.Package
quickpull = C.closedOpen "quickpull" [0,4] [0,5]

contravariant :: C.Package
contravariant = C.closedOpen "contravariant" [1,2] [1,3]

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
  , contravariant
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
