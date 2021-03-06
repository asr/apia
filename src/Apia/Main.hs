
-- | Apia: A program for proving first-order theorems written in the
-- dependently typed language Agda using first-order automatic theorem
-- provers.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Main
  ( main  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.TypeChecking.Monad.Base
  ( Definitions
  , Interface(iSignature)
  , sigDefinitions
  )

import Agda.Utils.Impossible ( catchImpossible )
import Agda.Utils.Lens       ( (^.) )

import Apia.ATPs      ( callATPs )
import Apia.CheckTPTP ( checkTPTP )
import Apia.Common    ( Lang(TPTP) )
import Apia.Dump      ( dumpTypes )

import Apia.Monad.Base
  ( modifyTDefs
  , runT
  , T
  , tCatch
  , tErr
  , TErr(MissingInputFile)
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

import Apia.Snapshot   ( snapshotTest )
import Apia.TPTP.Files ( createConjectureTPTPFile )
import Apia.TPTP.Types ( ConjectureSet, GeneralRoles )

import Apia.Translation ( conjecturesToAFors, generalRolesToAFors )

import Apia.Utils.AgdaAPI.Interface ( getImportedInterfaces, readInterface )
import Apia.Utils.Monad             ( failureMsg, pair )
import Apia.Utils.Version           ( progNameVersion )

import Control.Monad.Reader ( ask )

import qualified Data.Text as T ( pack )

import qualified Data.HashMap.Strict as HashMap ( unions )

import System.Exit ( exitFailure, exitSuccess )

------------------------------------------------------------------------------

translation ∷ FilePath → T (GeneralRoles, [ConjectureSet])
translation agdaFile = do
  -- Getting the interface for the top level module.
  i ← readInterface agdaFile

  -- Getting the interfaces for the imported modules.
  iInterfaces ← getImportedInterfaces i

  let topLevelDefs ∷ Definitions
      topLevelDefs = iSignature i ^. sigDefinitions

      importedDefs ∷ [Definitions]
      importedDefs = map (\t → iSignature t ^. sigDefinitions) iInterfaces

      allDefs ∷ Definitions
      allDefs = HashMap.unions (topLevelDefs : importedDefs)

  reportSLn "translation" 20 $ show allDefs

  -- We add @allDefs@ the translation monad state.
  modifyTDefs allDefs

  pair generalRolesToAFors $ conjecturesToAFors topLevelDefs

-- | The main function.
runApia ∷ T ()
runApia = do
  opts ← ask
  case () of
    _ | optHelp opts → liftIO printUsage

      | optVersion opts → liftIO $ do
          v ← progNameVersion
          putStrLn $ T.pack v

      | otherwise → do

        file ← case optInputFile opts of
                 Nothing → tErr MissingInputFile
                 Just f  → return f

        case () of
          _ | -- Dump Agda types information to stdout.
              optDumpTypes opts → dumpTypes file

            | otherwise → do

              -- The ATP pragmas are translated to annotated formulae.
              allAFs ← translation file

              -- Creation of the TPTP files.
              tptpFiles ←
                if optLang opts == TPTP
                then mapM (createConjectureTPTPFile (fst allAFs)) (snd allAFs)
                else return []

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
  r ∷ Either TErr () ← runT $ runApia `tCatch` \err →
    do liftIO $ failureMsg err
       tErr err

  case r of
    Right _ → exitSuccess
    Left  _ → exitFailure
  `catchImpossible` \e →
    do hPrint stderr e
       exitFailure
