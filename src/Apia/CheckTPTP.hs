
-- | Check the generated TPTP file using the tptp4X program.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.CheckTPTP ( checkTPTP ) where

-----------------------------------------------------------------------------

import Apia.Prelude

import Apia.Monad.Base
  ( askTOpt
  , T
  , tErr
  , TErr(MissingTPTP4XCommand, TPTP4XErrorWarning)
  )

import Apia.Monad.Utils   ( findExecutableErr )
import Apia.Options       ( Options(optWithtptp4X) )

import System.Exit    ( ExitCode(ExitSuccess, ExitFailure) )
import System.Process ( readProcessWithExitCode )

-----------------------------------------------------------------------------

-- | Check the generated TPTP file using the @tptp4X@ program.
checkTPTP ∷ FilePath → T ()
checkTPTP file = do

  tptp4XExec ← askTOpt optWithtptp4X

  -- TODO (2017-01-03): Missing error in the test-suite.
  findExecutableErr tptp4XExec $ MissingTPTP4XCommand tptp4XExec

  (exitCode, out, _) ← liftIO $
    readProcessWithExitCode tptp4XExec
                            ["-ftptp", "-umachine" , "-w", "-q3", file]
                            []

  case exitCode of
    ExitFailure _ → tErr $ TPTP4XErrorWarning file tptp4XExec out

    -- TODO (2017-01-03). How add a test case for this case?
    ExitSuccess →
      when ("WARNING" `isInfixOf` out) $ tErr $
        TPTP4XErrorWarning file tptp4XExec out
