-----------------------------------------------------------------------------
-- |
-- Module      : Apia.ATPs
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Call the automatic theorem provers for first-order logic (ATPs).
-----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.ATPs
  ( ATP  -- Required by Haddock.
  , callATPs
  ) where

------------------------------------------------------------------------------
-- Haskell imports

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ( (<$>) )
#endif

import Control.Exception.Base  ( catch, IOException )
-- TODO (09 July 2015): Use @evaluate@ from Control.Exception.Base.
import Control.Exception       ( evaluate )
import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Monad           ( when )
import Control.Monad.IO.Class  ( MonadIO(liftIO) )

import Data.List  ( isInfixOf )
import Data.Maybe ( fromMaybe, isNothing )

import Safe ( initDef )

import System.Directory ( findExecutable )
import System.FilePath  ( dropFileName, replaceExtension )
import System.IO        ( hGetContents )

import System.Process
  ( callProcess
  , CmdSpec(RawCommand)
  , createProcess
  , CreateProcess
    ( close_fds
    , create_group
    , CreateProcess
    , cmdspec
    , cwd
#if MIN_VERSION_process(1,2,0)
    , delegate_ctlc
#endif
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
-- Apia imports

import Apia.Monad.Base    ( askTOpt, T )
import Apia.Monad.Reports ( reportS )

import Apia.Options ( Options(optATP, optTime, optUnprovenNoError, optVampireExec) )

import qualified Apia.Utils.Except as E

#include "undefined.h"

------------------------------------------------------------------------------
-- | The ATPs.
data ATP = E
         | Equinox
         | IleanCoP
         | Metis
         | SPASS
         | Vampire
         | Z3
           deriving Show

atpExec ∷ ATP → T String
atpExec E        = return "eprover"
atpExec Equinox  = return "equinox"
atpExec IleanCoP = return "ileancop.sh"
atpExec Metis    = return "metis"
atpExec SPASS    = return "SPASS"
atpExec Vampire  = askTOpt optVampireExec
atpExec Z3       = return "z3"

optATP2ATP ∷ String → T ATP
optATP2ATP "e"        = return E
optATP2ATP "equinox"  = return Equinox
optATP2ATP "ileancop" = return IleanCoP
optATP2ATP "metis"    = return Metis
optATP2ATP "spass"    = return SPASS
optATP2ATP "vampire"  = return Vampire
optATP2ATP "z3"       = return Z3
optATP2ATP other      = E.throwE $ "ATP " ++ other ++ " unknown"

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
-- Metis 2.3 (release 20150303).
atpOk Metis = "SZS status Theorem"
-- SPASS 3.7.
atpOk SPASS = "Proof found"
-- Vampire 0.6 (revision 903).
atpOk Vampire = "Termination reason: Refutation\n"
-- Z3 4.4.0.
atpOk Z3 = "unsat"

atpVersion ∷ ATP → T String
 -- No version option in Equinox.
atpVersion Equinox = do
  exec ← atpExec Equinox
  liftIO $ fmap (initDef (__IMPOSSIBLE__) . takeWhile (/= '\n'))
                (readProcess exec ["--help"] "")
-- No version option in ileanCoP.
atpVersion IleanCoP = return $ show IleanCoP
-- No version option in SPASS.
atpVersion SPASS = return $ show SPASS
atpVersion atp = do
  exec ← atpExec atp
  liftIO $ initDef (__IMPOSSIBLE__) <$> readProcess exec ["--version"] ""

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
        else E.throwE $ "the ATP " ++ eVersion ++ " is not supported"

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
atpArgs Vampire timeLimit file = return [ "--mode", "casc"
                                        , "-t", show timeLimit
                                        , "--input_file", file
                                        ]

atpArgs Z3 timeLimit file = return [ "-T:" ++ show timeLimit
                                   , file
                                   ]

tptp2X ∷ String
tptp2X = "tptp2X"

-- tptp2X from TPTP 6.1.0 is returing exit status 1 instead of 0
-- (bug), so we need to handle the exception raised.
createSMT2file ∷ FilePath → T ()
createSMT2file file = do

  e ← liftIO $ findExecutable tptp2X
  when (isNothing e) $ E.throwE $
    "the " ++ tptp2X ++ " command from the TPTP library does not exist " ++
    "and it is required for using Z3 as an first-order ATP"

  let dir ∷ String
      dir = dropFileName file

  liftIO $
    callProcess tptp2X [ "-q2", "-fsmt2", "-d", dir, file ]
      `catch` (\(_ ∷ IOException) → return ())

smt2Ext ∷ String
smt2Ext = ".smt2"

runATP ∷ ATP → MVar (Bool, ATP) → Int → FilePath → T ProcessHandle
runATP atp outputMVar timeLimit fileTPTP = do

  file ← case atp of
           Z3 → do
             createSMT2file fileTPTP
             return (replaceExtension fileTPTP smt2Ext)

           _  → return fileTPTP

  args ∷ [String] ← atpArgs atp timeLimit file
  cmd  ∷ String   ← atpExec atp

  e ← liftIO $ findExecutable cmd
  when (isNothing e) $ E.throwE $
    "the command " ++ cmd ++ " associated with " ++ show atp
    ++ " does not exist.\nYou can use the command-line option --atp=NAME "
    ++ "to avoid call some ATP"

  -- To create the ATPs process we follow the ideas used by
  -- @System.Process.proc@.
  (_, outputH, _, atpPH) ← liftIO $
    createProcess CreateProcess
                    { cmdspec       = RawCommand cmd args
                    , cwd           = Nothing
                    , env           = Nothing
                    , std_in        = Inherit
                    , std_out       = CreatePipe
                    , std_err       = Inherit
                    , close_fds     = False
                    , create_group  = True
#if MIN_VERSION_process(1,2,0)
                    , delegate_ctlc = False
#endif
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
          msg = "the ATP(s) did not prove the conjecture in " ++ file
      ifM (askTOpt optUnprovenNoError)
          (liftIO $ putStrLn msg)
          (E.throwE msg)
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
