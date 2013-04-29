------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- apia: A program for proving first-order theorems written in the
-- dependently typed language Agda using first-order automatic theorem
-- provers.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main ( main )
where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad        ( unless )
import Control.Monad.Error  ( MonadError(catchError, throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.Trans  ( MonadIO(liftIO) )

import qualified Data.HashMap.Strict as HashMap ( unions )

import System.Exit ( exitFailure, exitSuccess )
import System.IO   ( hPrint, stderr )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.TypeChecking.Monad.Base
  ( Definitions
  , Interface(iPragmaOptions, iSignature)
  , Signature(sigDefinitions)
  )

import Agda.Utils.Impossible ( catchImpossible )

------------------------------------------------------------------------------
-- Local imports

import AgdaInternal.Interface ( getImportedInterfaces, myReadInterface )
import ATP                    ( callATPs )

import CheckTPTP ( checkTPTP )

import Dump ( dumpAgdai, dumpTypes )

import Monad.Base
  ( modifyDefs
  , modifyPragmaOptions
  , runT
  , T
  )

import Monad.Reports ( reportSLn )

import Options
  ( Options(optDumpAgdai
           , optDumpTypes
           , optHelp
           , optInputFile
           , optNotCheck
           , optOnlyFiles
           , optSnapshotTest
           , optVersion
           )
  , printUsage
  )

import Snapshot         ( snapshotTest )
import TPTP.Files       ( createConjectureFile )
import TPTP.Translation ( conjecturesToAFs, generalRolesToAFs )
import TPTP.Types       ( ConjectureSet, GeneralRoles )
import Utils.Monad      ( failureMsg, pair )
import Utils.Version    ( progNameVersion )

#include "undefined.h"

------------------------------------------------------------------------------

translation ∷ FilePath → T (GeneralRoles, [ConjectureSet])
translation agdaFile = do
  -- Getting the interface for the top level module.
  i ← myReadInterface agdaFile

  -- Getting the interfaces for the imported modules.
  iInterfaces ← getImportedInterfaces i

  let topLevelDefs ∷ Definitions
      topLevelDefs = sigDefinitions $ iSignature i

      importedDefs ∷ [Definitions]
      importedDefs = map (sigDefinitions . iSignature) iInterfaces

      allDefs ∷ Definitions
      allDefs = HashMap.unions (topLevelDefs : importedDefs)

  reportSLn "translation" 20 $ show allDefs

  -- We add @allDefs@ and the interface pragma options to the state.
  modifyDefs allDefs
  modifyPragmaOptions $ concat $ iPragmaOptions i

  pair generalRolesToAFs $ conjecturesToAFs topLevelDefs

-- | The main function.
runAgda2ATP ∷ T ()
runAgda2ATP = do
  opts ← ask
  case () of
    _ | optHelp opts    → liftIO printUsage
      | optVersion opts → liftIO $ progNameVersion >>= putStrLn
      | otherwise       → do

        file ← case optInputFile opts of
                 Nothing → throwError "Missing input file (try --help)"
                 Just f  → return f

        case () of
          _ | -- Dump the Agda interface file to stdout.
              optDumpAgdai opts → dumpAgdai file
            | -- Dump type information to stdout.
              optDumpTypes opts → dumpTypes file
            | otherwise → do

              -- The ATP pragmas are translated to TPTP annotated formulae.
              allAFs ← translation file

              -- Creation of the TPTP files.
              tptpFiles ← mapM (createConjectureFile (fst allAFs)) (snd allAFs)

              -- Check the generated TPTP files using the tptp4X
              -- program from the TPTP library
              unless (optNotCheck opts) $ mapM_ checkTPTP tptpFiles

              case () of
                _ | -- Run the snapshot test.
                    optSnapshotTest opts → mapM_ snapshotTest tptpFiles

                  | -- The ATPs systems are called on the TPTP files.
                    not (optOnlyFiles opts) → mapM_ callATPs tptpFiles

                  | otherwise → return ()

-- | Main.
main ∷ IO ()
main = do
  -- Adapted from @Agda.Main.main@.
  r ∷ Either String () ← runT $ runAgda2ATP `catchError` \err →
    do liftIO $ failureMsg err
       throwError err

  case r of
    Right _ → exitSuccess
    Left  _ → exitFailure
  `catchImpossible` \e →
    do hPrint stderr e
       exitFailure
