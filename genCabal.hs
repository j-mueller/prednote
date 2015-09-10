-- Generates the Cabal file for prednote.
-- Written to use version 0.14.2.0 of the Cartel
-- library.

module Main where

import Cartel
import Control.Applicative

atLeast :: NonEmptyString -> [Word] -> Package
atLeast name ver = package name (gtEq ver)

versionInts :: [Word]
versionInts = [0,36,0,4]

base :: Package
base = closedOpen "base" [4,7] [5]

rainbow :: Package
rainbow = atLeast "rainbow" [0,26]

text :: Package
text = atLeast "text" [0,11,2,0]

containers :: Package
containers = atLeast "containers" [0,4,2,1]

quickcheck :: Package
quickcheck = atLeast "QuickCheck" [2,7]

tasty :: Package
tasty = atLeast "tasty" [0,10]

tastyQuickcheck :: Package
tastyQuickcheck = atLeast "tasty-quickcheck" [0,8]

tastyTh :: Package
tastyTh = atLeast "tasty-th" [0,1]

bytestring :: Package
bytestring = atLeast "bytestring" [0,10]

properties :: Properties
properties = blank
  { name = "prednote"
  , version = versionInts
  , cabalVersion = Just (1,18)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "Copyright 2013-2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/prednote"
  , bugReports = "http://www.github.com/massysett/prednote/issues"
  , category = "Data"
  , synopsis = "Evaluate and display trees of predicates"
  , description =
    [ "Build and evaluate trees of predicates. For example, you might build"
    , "a predicate of the type Int -> Bool. You do this by assembling"
    , "several predicates into a tree. You can then verbosely evaluate"
    , "this tree, showing why a particular result is reached."
    , ""
    , "prednote also provides modules to test several subjects against a"
    , "given predicate, and to parse infix or RPN expressions into a tree of"
    , "predicates."
    ]
  , extraSourceFiles =
    [ "README.md"
    , "changelog"
    , "genCabal.hs"
    ]

  }

ghcOpts :: [String]
ghcOpts = ["-Wall"]

-- Dependencies

split :: Package
split = atLeast "split" [0,2,2]

contravariant :: Package
contravariant = atLeast "contravariant" [1,2]

transformers :: Package
transformers = atLeast "transformers" [0,3,0,0]

libDepends :: [Package]
libDepends =
  [ base
  , rainbow
  , split
  , text
  , containers
  , contravariant
  , transformers
  , bytestring
  ]

library
  :: [String]
  -- ^ Library modules
  -> [LibraryField]
library ms =
  [ exposedModules ms
  , buildDepends libDepends
  , hsSourceDirs ["lib"]
  , ghcOptions ghcOpts
  , haskell2010
  ]

tests
  :: FlagName
  -- ^ Visual-tests flag
  -> [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> (Section, Section)
  -- ^ The prednote-tests test suite, and the prednote-visual-tests
  -- executable
tests fl ls ts =
  ( testSuite "prednote-tests" $
    commonTestOpts ls ts ++
    [ mainIs "prednote-tests.hs"
    , exitcodeStdio
    ]
  , testSuite "prednote-visual-tests" $
    [ mainIs "prednote-visual-tests.hs"
    , exitcodeStdio
    ] ++ commonTestOpts ls ts
  )

commonTestOpts
  :: HasBuildInfo a
  => [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> [a]
commonTestOpts ls ts =
  [ hsSourceDirs ["lib", "tests"]
  , otherModules (ls ++ ts)
  , ghcOptions ghcOpts
  , haskell2010
  , otherExtensions ["TemplateHaskell"]
  , buildDepends
    $ tasty : tastyQuickcheck : tastyTh : quickcheck : libDepends
  ]

visualTests :: Applicative m => Betsy m FlagName
visualTests = makeFlag "visual-tests" $ FlagOpts
  { flagDescription = "Build the prednote-visual-tests executable"
  , flagDefault = False
  , flagManual = True
  }

github :: Section
github = githubHead "massysett" "prednote"

main :: IO ()
main = defaultMain $ do
  fl <- visualTests
  libMods <- modules "lib"
  testMods <- modules "tests"
  let (tsts, vis) = tests fl libMods testMods
      lib = library libMods
      repo = githubHead "massysett" "prednote"
  return (properties, lib, [tsts, vis, github])
