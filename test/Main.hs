{-# LANGUAGE UnicodeSyntax #-}

module Main ( main ) where

import qualified Data.Text as T

import Test.Tasty                    ( testGroup, TestTree )
import Test.Tasty.Silver             ( findByExtension, goldenVsProg )
import Test.Tasty.Silver.Interactive ( defaultMain )

------------------------------------------------------------------------------

apiaBIN ∷ String
apiaBIN = "../dist/build/apia/apia"

agdaBIN ∷ String
agdaBIN = "agda"

nonConjecturesTests ∷ TestTree
nonConjecturesTests = testGroup "non-conjectures"
  [ helpOption
  , onlyFilesOption
  , verboseOption
  , unprovenConjectureNoErrorOption
  , interface
  , dumpTypesOption
  , issue18
  , onlineATP
  ]

  where
  nonConjecturesPath ∷ String
  nonConjecturesPath = "succeed/non-conjectures/"

  helper ∷ String → String → [String] → TestTree
  helper exec name arg = goldenVsProg testName goldenFile exec args T.empty
    where
      testName ∷ String
      testName = nonConjecturesPath ++ name

      goldenFile ∷ String
      goldenFile = testName ++ ".golden"

      args ∷ [String]
      args = ("-i" ++ nonConjecturesPath) : arg

  helpOption ∷ TestTree
  helpOption = helper apiaBIN "help-option" ["--help"]

  onlyFilesOption ∷ TestTree
  onlyFilesOption = helper apiaBIN "only-files-option"
    [ "--atp=e"
    , "--only-files"
    , nonConjecturesPath ++ "TrivialTheorem.agda"
    ]

  verboseOption ∷ TestTree
  verboseOption = helper apiaBIN "verbose-option"
    [ "--atp=e"
    , "--only-files"
    , "-v1"
    , nonConjecturesPath ++ "TrivialTheorem.agda"
    ]

  unprovenConjectureNoErrorOption ∷ TestTree
  unprovenConjectureNoErrorOption =
    helper apiaBIN "unproven-conjecture-no-error-option"
      [ "--atp=e"
      , "--unproven-conjecture-no-error"
      , nonConjecturesPath ++ "NoTheorem.agda"
      ]

  interface ∷ TestTree
  interface = helper agdaBIN "interface"
    [ "-v"
    , "main:50"
    ,  nonConjecturesPath ++ "Foo.agda"
    ]

  dumpTypesOption ∷ TestTree
  dumpTypesOption = helper apiaBIN "dump-types-option"
    [ "--dump-types"
    , nonConjecturesPath ++ "Foo.agda"
    ]

  issue18 ∷ TestTree
  issue18 = helper apiaBIN "issue-18"
    [ "--dump-types"
    , nonConjecturesPath ++ "Issue18.agda"
    ]

  onlineATP ∷ TestTree
  onlineATP = helper apiaBIN "online-atp"
    [ "--atp=online-e"
    , nonConjecturesPath ++ "TrivialTheorem.agda"
    ]

allTests ∷ IO TestTree
allTests = return $ testGroup "tests" [ nonConjecturesTests ]

main ∷ IO ()
main = defaultMain =<< allTests
