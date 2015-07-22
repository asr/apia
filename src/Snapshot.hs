------------------------------------------------------------------------------
-- |
-- Module      : Snapshot
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Snapshot test.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Snapshot ( snapshotTest ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Safe.Exact ( dropExact )

import System.FilePath ( combine, joinPath, splitPath )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.FileName ( doesFileExistCaseSensitive )
import Agda.Utils.Monad    ( ifM, unlessM, whenM )

------------------------------------------------------------------------------
-- Apia imports

import Monad.Base ( askTOpt, T )

import Options
  ( Options(optOutputDir, optSnapshotDir, optSnapshotNoError)
  )

import Utils.Directory ( notEqualFiles )

import qualified Utils.Except as E

------------------------------------------------------------------------------

-- | Compare the generated TPTP files against a snapshot of them in
-- the directory indicated by the flag @--snapshot-dir@.
snapshotTest ∷ FilePath → T ()
snapshotTest file = do
  outputDir   ← askTOpt optOutputDir
  snapshotDir ← askTOpt optSnapshotDir

  if outputDir == snapshotDir
    then E.throwE "the options `--output-dir' and `--snapshot-dir' cannot be the same"
    else do
      -- The original file without the output directory.
      let auxFile ∷ FilePath
          auxFile = joinPath $ dropExact (length $ splitPath outputDir) $ splitPath file

          snapshotFile ∷ FilePath
          snapshotFile = combine snapshotDir auxFile

      unlessM (liftIO $ doesFileExistCaseSensitive snapshotFile) $ E.throwE $
        "the file " ++ snapshotFile ++ " does not exist"

      whenM (liftIO $ notEqualFiles file snapshotFile) $ do
        let msg ∷ String
            msg = "the files are different:\n" ++ file ++ "\n" ++ snapshotFile

        ifM (askTOpt optSnapshotNoError)
            (liftIO $ putStrLn msg)
            (E.throwE msg)
