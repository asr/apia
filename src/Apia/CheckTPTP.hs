
-- | Check the generated TPTP file using the tptp4X program.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.CheckTPTP ( checkTPTP ) where

-----------------------------------------------------------------------------

import Apia.Prelude

import Apia.Monad.Base
  ( askTOpt
  , checkExecutable
  , T
  , tError
  , TError(MissingTPTP4XCommand, TPTP4XErrorWarning)
  )

import Apia.Options ( Options(optWithtptp4X) )

import System.Exit    ( ExitCode(ExitSuccess, ExitFailure) )
import System.Process ( readProcessWithExitCode )

-----------------------------------------------------------------------------

-- | Check the generated TPTP file using the @tptp4X@ program.
checkTPTP ∷ FilePath → T ()
checkTPTP file = do

  tptp4XExec ← askTOpt optWithtptp4X

  -- TODO (2017-01-03): Missing error in the test-suite.
  checkExecutable tptp4XExec $ MissingTPTP4XCommand tptp4XExec

  (exitCode, out, _) ← liftIO $
    readProcessWithExitCode tptp4XExec
                            ["-ftptp", "-umachine" , "-w", "-q3", file]
                            []

  case exitCode of
    ExitFailure _ → tError $ TPTP4XErrorWarning file tptp4XExec out

    -- TODO (2017-01-03). How add a test case for this case?
    ExitSuccess →
      when ("WARNING" `isInfixOf` out) $ tError $
        TPTP4XErrorWarning file tptp4XExec out
