------------------------------------------------------------------------------
-- |
-- Module      : Apia.Snapshot
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Snapshot test.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Snapshot ( snapshotTest ) where

------------------------------------------------------------------------------

import Agda.Utils.FileName ( doesFileExistCaseSensitive )
import Agda.Utils.Monad    ( ifM, unlessM, whenM )

import Apia.Monad.Base ( askTOpt, T )

import Apia.Options
  ( Options(optOutputDir, optSnapshotDir, optSnapshotNoError)
  )

import qualified Apia.Utils.Except as E

import Apia.Utils.Directory   ( notEqualFiles )
import Apia.Utils.PrettyPrint ( (<>), Doc, prettyShow, squotes, text )

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Safe.Exact             ( dropExact )
import System.FilePath        ( combine, joinPath, splitPath )

------------------------------------------------------------------------------

-- | Compare the generated TPTP files against a snapshot of them in
-- the directory indicated by the flag @--snapshot-dir@.
snapshotTest ∷ FilePath → T ()
snapshotTest file = do
  outputDir   ← askTOpt optOutputDir
  snapshotDir ← askTOpt optSnapshotDir

  if outputDir == snapshotDir
    then E.throwE $ text "the " <> squotes "--output-dir"
                    <> text " and " <> squotes "--snapshot-dir"
                    <> " options cannot be the same"
    else do
      -- The original file without the output directory.
      let auxFile ∷ FilePath
          auxFile = joinPath $ dropExact (length $ splitPath outputDir) $ splitPath file

          snapshotFile ∷ FilePath
          snapshotFile = combine snapshotDir auxFile

      unlessM (liftIO $ doesFileExistCaseSensitive snapshotFile) $ E.throwE $
        text "the file " <> text snapshotFile <> " does not exist"

      whenM (liftIO $ notEqualFiles file snapshotFile) $ do
        let msg ∷ Doc
            msg = text "the files are different:\n"
                  <> text file <> text "\n" <> text snapshotFile

        ifM (askTOpt optSnapshotNoError)
            (liftIO $ putStrLn $ prettyShow msg)
            (E.throwE msg)
