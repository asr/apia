------------------------------------------------------------------------------
-- |
-- Module      : Snapshot
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Snapshot test.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Snapshot ( snapshotTest ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad.Error ( MonadError(throwError) )
import Control.Monad.Trans ( liftIO )

import System.FilePath ( combine, joinPath, splitPath )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.FileName ( doesFileExistCaseSensitive )
import Agda.Utils.Monad    ( ifM, unlessM, whenM )

------------------------------------------------------------------------------
-- Local imports

import Monad.Base ( askTOpt, T )

import Options
  ( Options(optOutputDir, optSnapshotDir, optSnapshotNoError)
  )

import Utils.File ( notEqualFiles )

------------------------------------------------------------------------------
-- | Compare the generated TPTP files against a snapshot of them in
-- the directory indicated by the flag @--snapshot-dir@.
snapshotTest ∷ FilePath → T ()
snapshotTest file = do
  outputDir   ← askTOpt optOutputDir
  snapshotDir ← askTOpt optSnapshotDir

  if outputDir == snapshotDir
    then throwError "The options `--output-dir' and `--snapshot-dir' cannot be the same"
    else do
      -- The original file without the output directory.
      let auxFile ∷ FilePath
          auxFile = joinPath $ drop (length $ splitPath outputDir) $ splitPath file

          snapshotFile ∷ FilePath
          snapshotFile = combine snapshotDir auxFile

      unlessM (liftIO $ doesFileExistCaseSensitive snapshotFile) $ throwError $
        "The file " ++ snapshotFile ++ " does not exist"

      whenM (liftIO $ notEqualFiles file snapshotFile) $ do
        let msg ∷ String
            msg = "The files are different:\n" ++ file ++ "\n" ++ snapshotFile

        ifM (askTOpt optSnapshotNoError)
            (liftIO $ putStrLn msg)
            (throwError msg)
