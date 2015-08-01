-----------------------------------------------------------------------------
-- |
-- Module      : Apia.CheckTPTP
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Check the generated TPTP file using the tptp4X program.
-----------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.CheckTPTP ( checkTPTP ) where

-----------------------------------------------------------------------------

import Apia.Monad.Base      ( askTOpt, T )
import Apia.Options         ( Options(optWithtptp4X) )
import Apia.Utils.Directory ( checkExecutable )

import qualified Apia.Utils.Except as E

import Control.Monad           ( when )
import Control.Monad.IO.Class  ( MonadIO(liftIO) )

import Data.List ( isInfixOf )

import System.Exit    ( ExitCode(ExitSuccess, ExitFailure) )
import System.Process ( readProcessWithExitCode )

-----------------------------------------------------------------------------

-- | Check the generated TPTP file using the @tptp4X@ program.
checkTPTP ∷ FilePath → T ()
checkTPTP file = do

  tptp4XExec ← askTOpt optWithtptp4X

  let msgError ∷ String
      msgError = "the " ++ tptp4XExec ++ " command from the TPTP library "
                 ++ "does not exist"

  checkExecutable tptp4XExec msgError

  (exitCode, out, _) ←
    liftIO $ readProcessWithExitCode tptp4XExec
                                     ["-ftptp", "-umachine" , "-w", file]
                                     []
  case exitCode of
    ExitFailure _ →
      E.throwE $ tptp4XExec ++ " found an error in the file " ++ file
                 ++ "\nPlease report this as a bug"

    -- TODO (11 December 2012). How add a test case for this case?
    ExitSuccess →
      when ("WARNING" `isInfixOf` out) $
        E.throwE $ tptp4XExec ++ " found a warning in the file " ++ file
                   ++ "\nPlease report this as a bug"