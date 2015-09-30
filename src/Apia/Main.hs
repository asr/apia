------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Apia: A program for proving first-order theorems written in the
-- dependently typed language Agda using first-order automatic theorem
-- provers.
------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main
  ( main  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Agda.TypeChecking.Monad.Base
  ( Definitions
  , Interface(iSignature)
  , Signature(sigDefinitions)
  )

import Agda.Utils.Impossible ( catchImpossible )

import Apia.ATPs      ( callATPs, selectedATPs )
import Apia.CheckTPTP ( checkTPTP )
import Apia.Dump      ( dumpTypes )

import Apia.Monad.Base
  ( modifyTDefs
  , runT
  , T
  )

import Apia.Monad.Reports ( reportSLn )

import Apia.Options
  ( Options( optCheck
           , optDumpTypes
           , optHelp
           , optInputFile
           , optLang
           , optOnlyFiles
           , optSnapshotTest
           , optVersion
           )
  , printUsage
  )

import Apia.Snapshot         ( snapshotTest )
import Apia.TPTP.Files       ( createConjectureTPTPFile )
import Apia.TPTP.Translation ( conjecturesToAFors, generalRolesToAFors )
import Apia.TPTP.Types       ( ConjectureSet, GeneralRoles )

import qualified Apia.Utils.Except as E

import Apia.Utils.AgdaAPI.Interface ( getImportedInterfaces, readInterface )
import Apia.Utils.PrettyPrint       ( Doc )

import Apia.Utils.Monad   ( failureMsg, pair )
import Apia.Utils.Version ( progNameVersion )

import Control.Monad              ( when )
import Control.Monad.IO.Class     ( MonadIO(liftIO) )
import Control.Monad.Trans.Class  ( MonadTrans(lift) )
import Control.Monad.Trans.Reader ( ask )

import qualified Data.HashMap.Strict as HashMap ( unions )

import System.Exit ( exitFailure, exitSuccess )
import System.IO   ( hPrint, stderr )

------------------------------------------------------------------------------

translation ∷ FilePath → T (GeneralRoles, [ConjectureSet])
translation agdaFile = do
  -- Getting the interface for the top level module.
  i ← readInterface agdaFile

  -- Getting the interfaces for the imported modules.
  iInterfaces ← getImportedInterfaces i

  let topLevelDefs ∷ Definitions
      topLevelDefs = sigDefinitions $ iSignature i

      importedDefs ∷ [Definitions]
      importedDefs = map (sigDefinitions . iSignature) iInterfaces

      allDefs ∷ Definitions
      allDefs = HashMap.unions (topLevelDefs : importedDefs)

  reportSLn "translation" 20 $ show allDefs

  -- We add @allDefs@ the translation monad state.
  modifyTDefs allDefs

  pair generalRolesToAFors $ conjecturesToAFors topLevelDefs

-- | The main function.
runApia ∷ T ()
runApia = do
  opts ← lift $ lift ask
  case () of
    _ | optHelp opts    → liftIO printUsage
      | optVersion opts → liftIO $ progNameVersion >>= putStrLn
      | otherwise       → do

        file ← case optInputFile opts of
                 Nothing → E.throwE "missing input file (try --help)"
                 Just f  → return f

        case () of
          _ | -- Dump Agda types information to stdout.
              optDumpTypes opts → dumpTypes file

            | otherwise → do

              -- The ATP pragmas are translated to annotated formulae.
              allAFs ← translation file

              -- The selected ATPs are added to the translation monad
              -- state.
              selectedATPs

              -- Creation of the TPTP files.
              tptpFiles ←
                mapM (createConjectureTPTPFile (optLang opts) (fst allAFs))
                     (snd allAFs)

              -- Check the generated TPTP files using the tptp4X
              -- program from the TPTP library.
              when (optCheck opts) $ mapM_ checkTPTP tptpFiles

              case () of
                _ | -- Run the snapshot test.
                    optSnapshotTest opts → mapM_ snapshotTest tptpFiles

                  | -- The ATPs systems are called on the TPTP files.
                    not (optOnlyFiles opts) → mapM_ callATPs tptpFiles

                  | otherwise → return ()

-- | Main.
main ∷ IO ()
main = do
  -- Adapted from @Agda.Main.main@. Requires -XScopedTypeVariables.
  r ∷ Either Doc () ← runT $ runApia `E.catchE` \err →
    do liftIO $ failureMsg err
       E.throwE err

  case r of
    Right _ → exitSuccess
    Left  _ → exitFailure
  `catchImpossible` \e →
    do hPrint stderr e
       exitFailure
