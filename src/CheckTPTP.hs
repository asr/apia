-----------------------------------------------------------------------------
-- |
-- Module      : ATP
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Check the generated TPTP file using the tptp4X program.
-----------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module CheckTPTP ( checkTPTP ) where

-- Haskell imports

import Control.Monad       ( when )
import Control.Monad.Error ( MonadError(throwError) )
import Control.Monad.Trans ( MonadIO(liftIO) )

import Data.List  ( isInfixOf )
import Data.Maybe ( isNothing )

import System.Directory ( findExecutable )
import System.Exit      ( ExitCode (ExitSuccess, ExitFailure) )
import System.Process   ( readProcessWithExitCode )

------------------------------------------------------------------------------
-- Local imports

import Monad.Base ( T )

-----------------------------------------------------------------------------

tptp4Xexec ∷ String
tptp4Xexec = "tptp4X"

-- | Check the generated TPTP file using the @tptp4X@ program.
checkTPTP ∷ FilePath → T ()
checkTPTP file = do
  e ← liftIO $ findExecutable tptp4Xexec
  when (isNothing e) $ throwError $
    "The " ++ tptp4Xexec ++ " command from the TPTP library does not exist"

  (exitCode, out, _) ←
    liftIO $ readProcessWithExitCode tptp4Xexec
                                     ["-ftptp", "-umachine" , "-w", file]
                                     []
  case exitCode of
    ExitFailure _ →
      throwError $ tptp4Xexec ++ " found an error in the file " ++ file
                   ++ "\nPlease report this as a bug"

    -- TODO (11 December 2012). How add a test case for this case?
    ExitSuccess →
      when ("WARNING" `isInfixOf` out) $
        throwError $ tptp4Xexec ++ " found a warning in the file " ++ file
                     ++ "\nPlease report this as a bug"
