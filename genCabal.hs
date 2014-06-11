-- Generates the Cabal file for prednote.
-- Written to use version 0.10.0.2 of the Cartel
-- library.

module Main where

import qualified Cartel as C

-- Dependencies

base :: C.Package
base = C.closedOpen "base" [4,5,0,0] [5]

contravariant :: C.Package
contravariant = C.closedOpen "contravariant" [0,2,0,1] [0,7]

rainbow :: C.Package
rainbow = C.closedOpen "rainbow" [0,14,0,0] [0,15]

split :: C.Package
split = C.closedOpen "split" [0,2,2] [0,3]

text :: C.Package
text = C.closedOpen "text" [0,11,2,0] [1,2]

quickcheck :: C.Package
quickcheck = C.closedOpen "QuickCheck" [2,6] [2,7]

properties :: C.Properties
properties = C.empty
  { C.prName = "prednote"
  , C.prVersion = C.Version [0,24]
  , C.prLicenseFile = "LICENSE"
  , C.prCopyright = "Copyright 2013-2014 Omari Norman"
  , C.prAuthor = "Omari Norman"
  , C.prMaintainer = "omari@smileystation.com"
  , C.prStability = "Experimental"
  , C.prHomepage = "http://www.github.com/massysett/prednote"
  , C.prBugReports = "http://www.github.com/massysett/prednote/issues"
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
  , C.prCategory = "Data"
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

repo :: C.Repository
repo = C.empty
  { C.repoVcs = C.Git
  , C.repoKind = C.Head
  , C.repoLocation = "git://github.com/massysett/prednote.git"
  , C.repoBranch = "master"
  }

ghcOptions :: [String]
ghcOptions = ["-Wall"]

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
    ]
  , C.hsSourceDirs ["lib"]
  , C.ghcOptions ghcOptions
  , C.defaultLanguage C.Haskell2010
  ]

testSuite :: C.TestSuite
testSuite = C.TestSuite "prednote-test"
  [ C.TestType C.ExitcodeStdio
  , C.TestMainIs "prednote-test.hs"
  , C.hsSourceDirs ["./", "lib"]
  , C.buildDepends
    [ base
    , contravariant
    , rainbow
    , text
    , quickcheck
    ]
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
  , C.cTestSuites = [testSuite]
  }

main :: IO ()
main = do
  mods <- C.modules "lib"
  C.render "genCabal.hs" $ cabal mods
