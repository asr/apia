{-# LANGUAGE UnicodeSyntax #-}

module Succeed.NonConjectures.Test
  ( allNonConjecturesTests
  , couldBeDisabledTests
  ) where

import Utils ( agdaBIN, apiaBIN )

import qualified Data.Text as T

import Test.Tasty               ( testGroup, TestTree )
import Test.Tasty.Silver.Filter ( RegexFilter(RFInclude) )
import Test.Tasty.Silver        ( goldenVsProg )

------------------------------------------------------------------------------
-- Auxiliary functions

nonConjecturesPath ∷ String
nonConjecturesPath = "test/Succeed/NonConjectures/"

helper ∷ String → String → [String] → TestTree
helper exec name arg = goldenVsProg testName goldenFile exec args T.empty
  where
    testName ∷ String
    testName = nonConjecturesPath ++ name

    goldenFile ∷ String
    goldenFile = testName ++ ".golden"

    args ∷ [String]
    args = ("-i" ++ nonConjecturesPath) : arg

------------------------------------------------------------------------------
-- Tests

dumpTypesOptionTest ∷ TestTree
dumpTypesOptionTest = helper apiaBIN "dump-types-option"
  [ "--dump-types"
  , nonConjecturesPath ++ "Foo.agda"
  ]

helpOptionTest ∷ TestTree
helpOptionTest = helper apiaBIN "help-option" ["--help"]

interfaceTest ∷ TestTree
interfaceTest = helper agdaBIN "interface"
  [ "-v 0"
  , "-v"
  , "main:50"
  ,  nonConjecturesPath ++ "Foo.agda"
  ]

issue18Test ∷ TestTree
issue18Test = helper apiaBIN "issue-18"
  [ "--dump-types"
  , nonConjecturesPath ++ "Issue18.agda"
  ]

onlineATPTest ∷ TestTree
onlineATPTest = helper apiaBIN "online-atp"
  [ "--atp=online-e"
  , nonConjecturesPath ++ "TrivialTheorem.agda"
  ]

onlyFilesOptionTest ∷ TestTree
onlyFilesOptionTest = helper apiaBIN "only-files-option"
  [ "--atp=e"
  , "--only-files"
  , nonConjecturesPath ++ "TrivialTheorem.agda"
  ]

verboseOptionTest ∷ TestTree
verboseOptionTest = helper apiaBIN "verbose-option"
  [ "--atp=e"
  , "--only-files"
  , "-v1"
  , nonConjecturesPath ++ "TrivialTheorem.agda"
  ]

unprovenConjectureNoErrorOptionTest ∷ TestTree
unprovenConjectureNoErrorOptionTest =
  helper apiaBIN "unproven-conjecture-no-error-option"
    [ "--atp=e"
    , "--unproven-conjecture-no-error"
    , nonConjecturesPath ++ "NoTheorem.agda"
    ]

------------------------------------------------------------------------------

-- Sometimes we need to disable these tests (i.e. on Travis) because
-- the golden files include full paths.
couldBeDisabledTests :: [RegexFilter]
couldBeDisabledTests = [ RFInclude "test/Succeed/NonConjectures/interface" ]

allNonConjecturesTests ∷ TestTree
allNonConjecturesTests = testGroup "non-conjectures"
  [ dumpTypesOptionTest
  , helpOptionTest
  , interfaceTest
  , issue18Test
  , onlineATPTest
  , onlyFilesOptionTest
  , verboseOptionTest
  , unprovenConjectureNoErrorOptionTest
  ]

