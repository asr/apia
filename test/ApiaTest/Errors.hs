{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module ApiaTest.Errors ( errorsTests ) where

import ApiaTest.Utils ( apiaBIN )

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative ((<$>))
#endif

import qualified Data.Text as T

import System.Directory ( doesFileExist )
import System.FilePath  ( replaceExtension )

import Test.Tasty        ( testGroup, TestTree )
import Test.Tasty.Silver ( findByExtension, goldenVsProg )

------------------------------------------------------------------------------

errorsPath ∷ String
errorsPath = "test/fail/errors/"

tastyTest ∷ FilePath → IO TestTree
tastyTest testFile = do
  let goldenFile ∷ FilePath
      goldenFile = replaceExtension testFile ".golden"

      flagsFile ∷ FilePath
      flagsFile = replaceExtension testFile ".flags"

  flagsExist ← doesFileExist flagsFile
  flags ← if flagsExist then words <$> readFile flagsFile else return []

  let args ∷ [String]
      args = [ "-itest/fail/errors"
             , testFile
             ] ++ flags

  return $ goldenVsProg testFile goldenFile apiaBIN args T.empty

errorsWithFiles ∷ IO TestTree
errorsWithFiles = do
  files ← findByExtension [".agda"] errorsPath
  testGroup "errors-with-files" <$> mapM tastyTest files

errorsWithoutFilesTests ∷ TestTree
errorsWithoutFilesTests = testGroup "errors-without-files"
  [ missingInputFile
  , noAgdaFile
  , onlyOneFile
  , unrecognizedOption
  , missingDir
  , negativeTimeout
  , incompleteVerbose
  ]

  where
  helper ∷ String → String → [String] → TestTree
  helper exec name arg = goldenVsProg testName goldenFile exec args T.empty
    where
      testName ∷ String
      testName = errorsPath ++ name

      goldenFile ∷ String
      goldenFile = testName ++ ".golden"

      args ∷ [String]
      args = ("-i" ++ errorsPath) : arg

  missingInputFile ∷ TestTree
  missingInputFile = helper apiaBIN "missing-input-file" []

  noAgdaFile ∷ TestTree
  noAgdaFile = helper apiaBIN "no-agda-file" [ "/tmp/NoFile.agda" ]

  onlyOneFile ∷ TestTree
  onlyOneFile = helper apiaBIN "only-one-file" [ "File1.agda", "File2.agda" ]

  unrecognizedOption ∷ TestTree
  unrecognizedOption = helper apiaBIN "unrecognized-option" [ "--xxx" ]

  missingDir ∷ TestTree
  missingDir = helper apiaBIN "missing-dir" [ "--include-path" ]

  negativeTimeout ∷ TestTree
  negativeTimeout = helper apiaBIN "negative-timeout" [ "--time=-10" ]

  incompleteVerbose ∷ TestTree
  incompleteVerbose = helper apiaBIN "incomplete-verbose" [ "--verbose=" ]

errorsTests ∷ IO TestTree
errorsTests = do
  errorsWithFilesTests ← errorsWithFiles
  return $ testGroup "errors" [ errorsWithoutFilesTests, errorsWithFilesTests ]
