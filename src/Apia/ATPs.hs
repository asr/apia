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
  , selectedATPs
  ) where

------------------------------------------------------------------------------

import Agda.Utils.Impossible ( Impossible(Impossible) , throwImpossible )
import Agda.Utils.Monad      ( ifM )

import Apia.Common
  ( ATP( CVC4
       , E
       , Equinox
       , IleanCoP
       , Metis
       , SPASS
       , Vampire
       , Z3
       )
  )

import Apia.Monad.Base    ( askTOpt, getTATPs, modifyTATPs, T )
import Apia.Monad.Reports ( reportS )

import Apia.Options
  ( Options( optATP
           , optTime
           , optUnprovenNoError
           , optWithCVC4
           , optWithE
           , optWithEquinox
           , optWithIleanCoP
           , optWithMetis
           , optWithSPASS
           , optWithtptp2X
           , optWithVampire
           , optWithZ3
           )
  )

import Apia.Utils.Directory   ( checkExecutable )

import Apia.Utils.PrettyPrint
  ( (<>)
  , Doc
  , Pretty(pretty)
  , prettyShow
  , squotes
  )

import qualified Apia.Utils.Except as E

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ( (<$>) )
#endif

import Control.Exception.Base  ( catch, evaluate, IOException )
import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Monad.IO.Class  ( MonadIO(liftIO) )

import Data.List  ( isInfixOf )
import Data.Maybe ( fromMaybe )

import Safe ( initDef )

import System.FilePath ( dropFileName, replaceExtension )
import System.IO       ( hGetContents )

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

#include "undefined.h"

------------------------------------------------------------------------------

atpExec ∷ ATP → T String
atpExec CVC4     = askTOpt optWithCVC4
atpExec E        = askTOpt optWithE
atpExec Equinox  = askTOpt optWithEquinox
atpExec IleanCoP = askTOpt optWithIleanCoP
atpExec Metis    = askTOpt optWithMetis
atpExec SPASS    = askTOpt optWithSPASS
atpExec Vampire  = askTOpt optWithVampire
atpExec Z3       = askTOpt optWithZ3

optATP2ATP ∷ String → T ATP
optATP2ATP "cvc4"     = return CVC4
optATP2ATP "e"        = return E
optATP2ATP "equinox"  = return Equinox
optATP2ATP "ileancop" = return IleanCoP
optATP2ATP "metis"    = return Metis
optATP2ATP "spass"    = return SPASS
optATP2ATP "vampire"  = return Vampire
optATP2ATP "z3"       = return Z3
optATP2ATP other =
  E.throwE $ pretty "ATP " <> pretty other <> pretty " unknown"

-- | Default ATPs.
defaultATPs ∷ [String]
defaultATPs = ["e", "equinox", "vampire"]

atpOk ∷ ATP → String
-- CVC4 1.4.
atpOk CVC4 = "SZS status Theorem"
-- E 1.9.
atpOk E = "Proof found!"
-- Equinox 5.0alpha (2010-06-29).
atpOk Equinox = "+++ RESULT: Theorem"
-- ileanCoP 1.3 beta1.
atpOk IleanCoP = "Intuitionistic Theorem"
-- Metis 2.3 (release 20160102).
atpOk Metis = "SZS status Theorem"
-- SPASS 3.7.
atpOk SPASS = "Proof found"
-- Vampire 0.6 (revision 903).
atpOk Vampire = "Termination reason: Refutation\n"
-- Z3 4.4.1.
atpOk Z3 = "unsat"

atpVersion ∷ ATP → T String
atpVersion CVC4 = do
  exec ← atpExec CVC4
  liftIO $ fmap ( drop (length ("This is " ∷ String))
                . takeWhile (/= '\n')
                . initDef (__IMPOSSIBLE__)
                )
                (readProcess exec ["--version"] "")
-- No `--version` option in Equinox.
atpVersion Equinox = do
  exec ← atpExec Equinox
  liftIO $ fmap (takeWhile (/= '\n') . initDef (__IMPOSSIBLE__))
                (readProcess exec ["--help"] "")
-- No `--version` option in ileanCoP.
atpVersion IleanCoP = return $ show IleanCoP
-- No `--version` option in SPASS.
atpVersion SPASS = return $ show SPASS
atpVersion atp = do
  exec ← atpExec atp
  liftIO $ initDef (__IMPOSSIBLE__) <$> readProcess exec ["--version"] ""

checkOutput ∷ ATP → String → Bool
checkOutput atp output = atpOk atp `isInfixOf` output

atpArgs ∷ ATP → Int → FilePath → T [String]

-- TODO (20 July 2015). The timeout is not working with precision.
atpArgs CVC4 timeout file =
  return [ "--lang=tptp"
         , "--strict-parsing"
         , "--tlimit=" ++ show (timeout * 1000)
         , file
         ]

atpArgs E timeout file = do
  eVersion ← atpVersion E
  if eVersion `elem` [ "E 1.2 Badamtam"
                     , "E 1.3 Ringtong"
                     , "E 1.4 Namring"
                     , "E 1.5 Pussimbing"
                     ]
    then return [ "--cpu-limit=" ++ show timeout
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
                         , "E 1.9 Sourenee"
                         ]
        then return [ "--auto"
                    , "--cpu-limit=" ++ show timeout
                    , "--memory-limit=Auto"
                    , "--output-level=0"
                    , "--tstp-format"
                    , file
                    ]
        -- This message is not included in the error test.
        else E.throwE $ pretty "the ATP " <> pretty eVersion
                        <> pretty " is not supported"

-- Equinox bug. Neither the option @--no-progress@ nor the option
-- @--verbose 0@ reduce the output.
atpArgs Equinox timeout file = return [ "--time", show timeout
                                      , file
                                      ]

-- N.B. The order of the ileanCoP arguments is fixed.
atpArgs IleanCoP timeout file = return [ file
                                       , show timeout
                                       ]

atpArgs Metis timeout file = return [ "--time-limit", show timeout
                                    , file
                                    ]

atpArgs SPASS timeout file = return [ "-PProblem=0"
                                    , "-PStatistic=0"
                                    , "-TimeLimit=" ++ show timeout
                                    , "-TPTP=1"
                                    , file
                                    ]

-- 25 July 2012. We don't know if Vampire has an option to reduce the
-- output.
atpArgs Vampire timeout file = return [ "--mode", "casc"
                                      , "-t", show timeout
                                      , "--input_file", file
                                      ]

atpArgs Z3 timeout file = return [ "-T:" ++ show timeout
                                 , file
                                 ]

-- The tptp2X program (from TPTP 6.3.0) is returing exit status 1
-- instead of 0 (bug), so we need to handle the exception raised.
createSMT2file ∷ FilePath → T ()
createSMT2file file = do

  tptp2XExec ← askTOpt optWithtptp2X

  let msgError ∷ Doc
      msgError = pretty "the " <> pretty tptp2XExec
                 <> pretty " command from the TPTP library "
                 <> pretty "does not exist and it is required for using "
                 <> pretty Z3 <> pretty " as an first-order ATP"

  checkExecutable tptp2XExec msgError

  let dir ∷ String
      dir = dropFileName file

  liftIO $
    callProcess tptp2XExec [ "-q2", "-fsmt2", "-d", dir, file ]
      `catch` (\(_ ∷ IOException) → return ())

smt2Ext ∷ String
smt2Ext = ".smt2"

-- | The selected ATPs by the user or the default ones.
selectedATPs ∷ T ()
selectedATPs = do
  atpsAux ← askTOpt optATP

  let atps ∷ [String]
      atps = if null atpsAux then defaultATPs else atpsAux

  mapM optATP2ATP atps >>= modifyTATPs

runATP ∷ ATP → MVar (Bool, ATP) → Int → FilePath → T ProcessHandle
runATP atp outputMVar timeout fileTPTP = do

  file ← case atp of
           Z3 → do
             createSMT2file fileTPTP
             return (replaceExtension fileTPTP smt2Ext)

           _  → return fileTPTP

  args ∷ [String] ← atpArgs atp timeout file
  cmd  ∷ String   ← atpExec atp

  let msgError ∷ Doc
      msgError = pretty "the " <> squotes cmd
                 <> pretty " command associated with "
                 <> pretty atp <> pretty " does not exist"

  checkExecutable cmd msgError

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

atpsAnswer ∷ [ATP] → MVar (Bool, ATP) → [ProcessHandle] → FilePath → Int →
             T ()
atpsAnswer atps outputMVar atpsPH file n =
  if n == length atps
    then do
      let msg ∷ Doc
          msg = pretty "the ATP(s) did not prove the conjecture in "
                <> pretty file

      ifM (askTOpt optUnprovenNoError)
          (liftIO $ putStrLn $ prettyShow msg)
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
  timeoutAux  ← askTOpt optTime
  outputMVar  ← liftIO (newEmptyMVar ∷ IO (MVar (Bool, ATP)))

  reportS "" 1 $ "Proving the conjecture in " ++ file
  -- See note [Timeout increse].
  let timeout ∷ Int
      timeout = round (fromIntegral timeoutAux * (1.1 ∷ Float))

  atps ← getTATPs
  reportS "" 20 $ "ATPs to be used: " ++ show atps

  atpsPH ∷ [ProcessHandle] ←
    mapM (\atp → runATP atp outputMVar timeout file) atps

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
