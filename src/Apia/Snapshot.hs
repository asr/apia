
-- | Snapshot test.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Snapshot ( snapshotTest ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Monad ( ifM, whenM )

import Apia.Monad.Base (
  askTOpt
  , T
  , tErr
  , TErr ( MissingFile
         , SnapshotDifferentFiles
         , SnapshotSameDirectory
         )
  , tWarn
  , TWarn(SnapshotDifferentFilesWarn)
  )

import Apia.Monad.Utils ( doesFileExistErr )

import Apia.Options
  ( Options(optOutputDir, optSnapshotDir, optSnapshotNoError)
  )

import Apia.Utils.IO ( notEqualFiles )

import Safe.Exact      ( dropExact )
import System.FilePath ( combine, joinPath, splitPath )

------------------------------------------------------------------------------

-- | Compare the generated TPTP files against a snapshot of them in
-- the directory indicated by the flag @--snapshot-dir@.
snapshotTest ∷ FilePath → T ()
snapshotTest file = do
  outputDir   ← askTOpt optOutputDir
  snapshotDir ← askTOpt optSnapshotDir

  if outputDir == snapshotDir
    then tErr SnapshotSameDirectory
    else do
      -- The original file without the output directory.
      let auxFile ∷ FilePath
          auxFile = joinPath $ dropExact (length $ splitPath outputDir) $ splitPath file

          snapshotFile ∷ FilePath
          snapshotFile = combine snapshotDir auxFile

      doesFileExistErr snapshotFile $ MissingFile snapshotFile

      whenM (liftIO $ notEqualFiles file snapshotFile) $
        ifM (askTOpt optSnapshotNoError)
            (tWarn $ SnapshotDifferentFilesWarn file snapshotFile)
            (tErr $ SnapshotDifferentFiles file snapshotFile)
