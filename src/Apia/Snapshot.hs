
-- | Snapshot test.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Snapshot ( snapshotTest ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.FileName ( doesFileExistCaseSensitive )
import Agda.Utils.Monad    ( ifM, unlessM, whenM )

import Apia.Monad.Base (
  askTOpt
  , T
  , tErr
  , TErr ( MissingFile
         , SnapshotDifferentFiles
         , SnapshotSameDirectory
         )
  )

import Apia.Options
  ( Options(optOutputDir, optSnapshotDir, optSnapshotNoError)
  )

import Apia.Utils.IO          ( notEqualFiles )
import Apia.Utils.PrettyPrint ( (<>), Doc, Pretty(pretty), prettyShow )

import qualified Data.Text as T ( pack )

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

      unlessM (liftIO $ doesFileExistCaseSensitive snapshotFile) $
        tErr $ MissingFile snapshotFile

      whenM (liftIO $ notEqualFiles file snapshotFile) $ do
        -- TODO (2017-01-03): Add Warning.
        let msg ∷ Doc
            msg = pretty "the files are different:\n"
                  <> pretty file <> pretty "\n" <> pretty snapshotFile

        ifM (askTOpt optSnapshotNoError)
            (putStrLn $ T.pack $ prettyShow msg)
            (tErr $ SnapshotDifferentFiles file snapshotFile)
