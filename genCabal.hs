-- Generates the Cabal file for prednote.
-- Written to use version 0.14.2.0 of the Cartel
-- library.

module Main where

import Cartel
import Control.Applicative

versionInts :: [Word]
versionInts = [0,32,0,6]

base :: Package
base = closedOpen "base" [4,7] [4,9]

rainbow :: Package
rainbow = nextBreaking "rainbow" [0,22]

text :: Package
text = closedOpen "text" [0,11,2,0] [1,3]

containers :: Package
containers = closedOpen "containers" [0,4,2,1] [0,6]

quickcheck :: Package
quickcheck = closedOpen "QuickCheck" [2,7] [2,9]

tasty :: Package
tasty = nextBreaking "tasty" [0,10]

tastyQuickcheck :: Package
tastyQuickcheck = nextBreaking "tasty-quickcheck" [0,8]

tastyTh :: Package
tastyTh = nextBreaking "tasty-th" [0,1]

bytestring :: Package
bytestring = nextBreaking "bytestring" [0,10]

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
  , testedWith = map (\ls -> (ghc, eq ls))
    [ [7,8,3], [7,10,1] ]
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
split = closedOpen "split" [0,2,2] [0,3]

contravariant :: Package
contravariant = closedOpen "contravariant" [1,2] [1,4]

transformers :: Package
transformers = closedOpen "transformers" [0,3,0,0] [0,5]

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
  , executable "prednote-visual-tests" $
    [ mainIs "prednote-visual-tests.hs"
    , condBlock (flag fl)
       ( buildable True, commonTestOpts ls ts)
      [ buildable False
      ]
    ]
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


main :: IO ()
main = defaultMain $ do
  fl <- visualTests
  libMods <- modules "lib"
  testMods <- modules "tests"
  let (tsts, vis) = tests fl libMods testMods
      lib = library libMods
      repo = githubHead "massysett" "prednote"
  return (properties, lib, [tsts, vis])
