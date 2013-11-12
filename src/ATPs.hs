-----------------------------------------------------------------------------
-- |
-- Module      : ATPs
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Call the automatic theorem provers for first-order logic (ATPs).
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module ATPs
  ( ATP  -- Required by Haddock.
  , callATPs
  )
  where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Exception       ( evaluate )
import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Monad           ( when )
import Control.Monad.Error     ( MonadError(throwError) )
import Control.Monad.Trans     ( MonadIO(liftIO) )

import Data.List  ( isInfixOf )
import Data.Maybe ( fromMaybe, isNothing )

import System.Directory ( findExecutable )
import System.IO        ( hGetContents )

import System.Process
  ( CmdSpec(RawCommand)
  , createProcess
  , CreateProcess
    ( create_group
    , CreateProcess
    , close_fds
    , cmdspec
    , cwd
    , env
    , std_err
    , std_in
    , std_out
    )
  , interruptProcessGroupOf
  , ProcessHandle
  , readProcess
  , StdStream(CreatePipe, Inherit)
  )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Impossible ( Impossible(Impossible) , throwImpossible )
import Agda.Utils.Monad      ( ifM )

------------------------------------------------------------------------------
-- Local imports

import Monad.Base    ( askTOpt, T )
import Monad.Reports ( reportS )

import Options ( Options(optATP, optTime, optUnprovenNoError, optVampireExec) )

#include "undefined.h"

------------------------------------------------------------------------------
-- | The ATPs.
data ATP = E
         | Equinox
         | IleanCoP
         | Metis
         | SPASS
         | Vampire
           deriving Show

atpExec ∷ ATP → T String
atpExec E        = return "eprover"
atpExec Equinox  = return "equinox"
atpExec IleanCoP = return "ileancop.sh"
atpExec Metis    = return "metis"
atpExec SPASS    = return "SPASS"
atpExec Vampire  = askTOpt optVampireExec

optATP2ATP ∷ String → T ATP
optATP2ATP "e"        = return E
optATP2ATP "equinox"  = return Equinox
optATP2ATP "ileancop" = return IleanCoP
optATP2ATP "metis"    = return Metis
optATP2ATP "spass"    = return SPASS
optATP2ATP "vampire"  = return Vampire
optATP2ATP other      = throwError $ "ATP " ++ other ++ " unknown"

-- | Default ATPs.
defaultATPs ∷ [String]
defaultATPs = ["e", "equinox", "vampire"]

atpOk ∷ ATP → String
-- E 1.2, E 1.3, E 1.4, E 1.5, E 1.6, E 1.7 and E 1.8.
atpOk E = "Proof found!"
-- Equinox 5.0alpha (2010-06-29).
atpOk Equinox = "+++ RESULT: Theorem"
-- ileanCoP 1.3 beta1.
atpOk IleanCoP = "Intuitionistic Theorem"
-- Metis 2.3 (release 20120927).
atpOk Metis = "SZS status Theorem"
-- SPASS 3.7.
atpOk SPASS = "Proof found"
-- Vampire 0.6 (revision 903).
atpOk Vampire = "Termination reason: Refutation\n"

atpVersion ∷ ATP → T String
 -- No version option in Equinox.
atpVersion Equinox = do
  exec ← atpExec Equinox
  liftIO $ fmap (init . takeWhile (/= '\n')) (readProcess exec ["--help"] "")
-- No version option in ileanCoP.
atpVersion IleanCoP = return $ show IleanCoP
-- No version option in SPASS.
atpVersion SPASS = return $ show SPASS
atpVersion atp = do
  exec ← atpExec atp
  liftIO $ fmap init (readProcess exec ["--version"] "")

checkOutput ∷ ATP → String → Bool
checkOutput atp output = atpOk atp `isInfixOf` output

atpArgs ∷ ATP → Int → FilePath → T [String]
atpArgs E timeLimit file = do
  eVersion ← atpVersion E
  if eVersion `elem` [ "E 1.2 Badamtam"
                     , "E 1.3 Ringtong"
                     , "E 1.4 Namring"
                     , "E 1.5 Pussimbing"
                     ]
    then return [ "--cpu-limit=" ++ show timeLimit
                , "--expert-heuristic=Auto"
                , "--memory-limit=Auto"
                , "--output-level=0"
                , "--term-ordering=Auto"
                , "--tstp-format"
                , file
                ]
    else
      if eVersion `elem` [ "E 1.6 Tiger Hill"
                         , "E 1.7 Jun Chiabari"
                         , "E 1.8-001 Gopaldhara"
                         ]
        then return [ "--auto"
                    , "--cpu-limit=" ++ show timeLimit
                    , "--memory-limit=Auto"
                    , "--output-level=0"
                    , "--tstp-format"
                    , file
                    ]
        -- This message is not included in the error test.
        else throwError $ "The ATP " ++ eVersion ++ " is not supported"

-- Equinox bug. Neither the option @--no-progress@ nor the option
-- @--verbose 0@ reduce the output.
atpArgs Equinox timeLimit file = return [ "--time", show timeLimit
                                        , file
                                        ]

-- N.B. The order of the ileanCoP arguments is fixed.
atpArgs IleanCoP timeLimit file = return [ file
                                         , show timeLimit
                                         ]

atpArgs Metis timeLimit file = return [ "--time-limit", show timeLimit
                                      , file
                                      ]

atpArgs SPASS timeLimit file = return [ "-PProblem=0"
                                      , "-PStatistic=0"
                                      , "-TimeLimit=" ++ show timeLimit
                                      , "-TPTP=1"
                                      , file
                                      ]

-- 25 July 2012. We don't know if Vampire has an option to reduce the
-- output.
atpArgs Vampire timeLimit file = return [ "--input_file", file
                                        , "--mode", "casc"
                                        , "-t", show timeLimit
                                        ]

runATP ∷ ATP → MVar (Bool, ATP) → Int → FilePath → T ProcessHandle
runATP atp outputMVar timeLimit file = do
  args ∷ [String] ← atpArgs atp timeLimit file
  cmd ∷ String   ← atpExec atp

  e ← liftIO $ findExecutable cmd
  when (isNothing e) $ throwError $
    "The command " ++ cmd ++ " associated with " ++ show atp
    ++ " does not exist.\nYou can use the command-line option --atp=NAME "
    ++ "to avoid call some ATP"

  -- To create the ATPs process we follow the ideas used by
  -- @System.Process.proc@.
  (_, outputH, _, atpPH) ← liftIO $
    createProcess CreateProcess
                    { cmdspec = RawCommand cmd args
                    , cwd = Nothing
                    , env = Nothing
                    , std_in = Inherit
                    , std_out = CreatePipe
                    , std_err = Inherit
                    , close_fds = False
                    , create_group = True
                    }
  output ← liftIO $ hGetContents $ fromMaybe (__IMPOSSIBLE__) outputH
  _      ← liftIO $ forkIO $
             evaluate (length output) >>
             putMVar outputMVar (checkOutput atp output, atp)

  return atpPH

atpsAnswer ∷ [String] → MVar (Bool, ATP) → [ProcessHandle] → FilePath → Int →
             T ()
atpsAnswer atps outputMVar atpsPH file n =
  if n == length atps
    then do
      let msg ∷ String
          msg = "The ATP(s) did not prove the conjecture in " ++ file
      ifM (askTOpt optUnprovenNoError)
          (liftIO $ putStrLn msg)
          (throwError msg)
    else do
      output ← liftIO $ takeMVar outputMVar
      atpWithVersion ← atpVersion (snd output)
      if fst output
        then do
          reportS "" 1 $ atpWithVersion ++ " proved the conjecture"
          liftIO $ mapM_ interruptProcessGroupOf atpsPH
        else do
          reportS "" 1 $ atpWithVersion ++ " *did not* prove the conjecture"
          atpsAnswer atps outputMVar atpsPH file (n + 1)

-- | The function 'callATPs' calls the selected 'ATP's on a TPTP conjecture.
callATPs ∷ FilePath → T ()
callATPs file = do
  atpsAux       ← askTOpt optATP
  timeLimitAux  ← askTOpt optTime
  outputMVar    ← liftIO (newEmptyMVar ∷ IO (MVar (Bool, ATP)))

  let atps ∷ [String]
      atps = if null atpsAux then defaultATPs else atpsAux

  reportS "" 1 $ "Proving the conjecture in " ++ file
  reportS "" 20 $ "ATPs to be used: " ++ show atps

  -- See note [Timeout increse].
  let timeLimit ∷ Int
      timeLimit = round (fromIntegral timeLimitAux * (1.1 ∷ Float))

  atpsPH ∷ [ProcessHandle] ←
    mapM optATP2ATP atps >>= mapM (\atp → runATP atp outputMVar timeLimit file)

  atpsAnswer atps outputMVar atpsPH file 0

------------------------------------------------------------------------------
-- Note [Timeout increse].

-- 12 June 2012: Hack. Running for example
--
-- @$ equinox --time 216 conjecture.tptp@
--
-- or
--
-- @$ apia --time=216 --atp=equinox conjecture.agda@
--
-- it is possible prove the theorem. But running for example
--
-- @$ apia --time=216 --atp=equinox --atp=vampire --atp=e conjecture.agda@
--
-- doesn't prove the theorem. I guess there is some overhead for
-- calling various ATPs from Apia. Therefore we increase internally
-- 10% the ATPs timeout.
