
-- | Call the automatic theorem provers for first-order logic (ATPs).

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.ATPs
  ( ATP  -- Required by Haddock.
  , callATPs
  , selectedATPs
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Impossible ( Impossible(Impossible) , throwImpossible )
import Agda.Utils.Monad      ( ifM )

import Apia.Common
  ( ATP( CVC4
       , E
       , Equinox
       , IleanCoP
       , Metis
       , OnlineATP
       , SPASS
       , Vampire
       , Z3
       )
  )

import Apia.Monad.Base    ( askTOpt, getTATPs, modifyTATPs, T )
import Apia.Monad.Reports ( reportS )

import Apia.Options
  ( extractATPs
  , Options( optATP
           , optTime
           , optUnprovenNoError
           , optWithCVC4
           , optWithE
           , optWithEquinox
           , optWithIleanCoP
           , optWithMetis
           , optWithOnlineATPs
           , optWithSPASS
           , optWithtptp4X
           , optWithVampire
           , optWithZ3
           )
  )

import Apia.Utils.Directory ( checkExecutable )

import Apia.Utils.PrettyPrint
  ( (<>)
  , Doc
  , Pretty(pretty)
  , prettyShow
  , scquotes
  , sspaces
  )

import qualified Apia.Utils.Except as E

import Control.Exception.Base  ( evaluate )
import Control.Concurrent      ( forkIO )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, takeMVar )

import qualified Data.Text as T ( pack )

import Safe ( initDef )

import System.FilePath ( replaceExtension )
import System.Exit     ( ExitCode(ExitSuccess, ExitFailure) )

import System.Process
  ( CmdSpec(RawCommand)
  , createProcess
  , CreateProcess
    ( close_fds
    , create_group
    , CreateProcess
    , cmdspec
    , cwd
    , env
    , std_err
    , std_in
    , std_out
#if MIN_VERSION_process(1,2,0)
    , delegate_ctlc
#endif
#if MIN_VERSION_process(1,3,0)
    , create_new_console
    , detach_console
    , new_session
#endif
#if MIN_VERSION_process(1,4,0)
    , child_group
    , child_user
#endif
    )
#if defined(linux_HOST_OS)
  , interruptProcessGroupOf
#endif
  , ProcessHandle
  , readProcess
  , readProcessWithExitCode
  , StdStream(CreatePipe, Inherit)
#if defined(darwin_HOST_OS)
  , terminateProcess
#endif
  )

#include "undefined.h"

------------------------------------------------------------------------------

atpExec ∷ ATP → T String
atpExec CVC4            = askTOpt optWithCVC4
atpExec E               = askTOpt optWithE
atpExec Equinox         = askTOpt optWithEquinox
atpExec IleanCoP        = askTOpt optWithIleanCoP
atpExec Metis           = askTOpt optWithMetis
atpExec (OnlineATP _)   = askTOpt optWithOnlineATPs
atpExec SPASS           = askTOpt optWithSPASS
atpExec Vampire         = askTOpt optWithVampire
atpExec Z3              = askTOpt optWithZ3

optATP2ATP ∷ String → T ATP
optATP2ATP "cvc4"           = return CVC4
optATP2ATP "e"              = return E
optATP2ATP "equinox"        = return Equinox
optATP2ATP "ileancop"       = return IleanCoP
optATP2ATP "metis"          = return Metis
optATP2ATP "spass"          = return SPASS
optATP2ATP "vampire"        = return Vampire
optATP2ATP "z3"             = return Z3
optATP2ATP other
  | "online-" `isPrefixOf` other = return $ OnlineATP other
  | otherwise =
    E.throwE $ pretty "the ATP " <> scquotes other <> pretty " is unknown"

atpOk ∷ ATP → String
-- CVC4 1.4.
atpOk CVC4 = "SZS status Theorem"
-- E 1.9.1-001
atpOk E = "Proof found!"
-- Equinox 5.0alpha (2010-06-29).
atpOk Equinox = "+++ RESULT: Theorem"
-- ileanCoP 1.3 beta1.
atpOk IleanCoP = "Intuitionistic Theorem"
-- Metis 2.3 (release 20161108).
atpOk Metis = "SZS status Theorem"
-- OnlineATPs 1.0.0
atpOk (OnlineATP _ ) = "Theorem"
-- SPASS 3.7.
atpOk SPASS = "Proof found"
-- Vampire 0.6 (revision 903).
atpOk Vampire = "Termination reason: Refutation\n"
-- Z3 version 4.5.0 - 64 bit
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
-- OnlineATPs has the option --version-atp=NAME
atpVersion atp@(OnlineATP name)  = do
  exec ← atpExec atp
  liftIO $ initDef (__IMPOSSIBLE__) <$>
    readProcess exec ["--version-atp=" ++ name ] ""

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
                         , "E 1.9.1 Sungma"
                         , "E 1.9.1-001 Sungma"
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

atpArgs (OnlineATP atp) timeout file  = return [ "--atp=" ++ atp
                                               , "--fof"
                                               , "--only-check"
                                               , "--time=" ++ show timeout
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

createSMT2file ∷ FilePath → T FilePath
createSMT2file tptpFile = do

  tptp4XExec ← askTOpt optWithtptp4X

  let errorMsg ∷ Doc
      errorMsg = pretty "the " <> pretty tptp4XExec
                 <> pretty " command from the TPTP library "
                 <> pretty "does not exist and it is required for using "
                 <> pretty Z3 <> pretty " as a first-order ATP"

  checkExecutable tptp4XExec errorMsg

  -- 2016-07-20: The `smt2` option is not documented on
  -- TPTP v6.4.0. Geoff Sutcliffe told us about this option via email.
  (exitCode, out, err) ← liftIO $
    readProcessWithExitCode tptp4XExec
                            [ "-fsmt2", tptpFile ]
                            []
  let errorOrWarningMsg ∷ Doc
      errorOrWarningMsg = pretty tptp4XExec
                          <> sspaces "found an error/warning in the file"
                          <> pretty tptpFile
                          <> sspaces "\nPlease report this as a bug\n\n"
                          <> pretty err

  case exitCode of
    ExitFailure _ → E.throwE errorOrWarningMsg

    ExitSuccess → do
      let smt2File ∷ FilePath
          smt2File = replaceExtension tptpFile smt2Ext

      liftIO $ writeFile smt2File out
      return smt2File

smt2Ext ∷ String
smt2Ext = ".smt2"

-- | The selected ATPs by the user or the default ones.
selectedATPs ∷ T ()
selectedATPs = do
  atps ← askTOpt optATP

  let errorMsg ∷ Doc
      errorMsg = pretty "at least you need to specify one ATP"

  let atps' ∷ [String]
      atps' = extractATPs atps

  if null atps'
    then E.throwE errorMsg
    else mapM optATP2ATP atps' >>= modifyTATPs

runATP ∷ ATP → MVar (Bool, ATP) → Int → FilePath → T ProcessHandle
runATP atp outputMVar timeout tptpFile = do

  file ← case atp of
           Z3 → createSMT2file tptpFile

           _  → return tptpFile

  args ∷ [String] ← atpArgs atp timeout file
  cmd  ∷ String   ← atpExec atp

  let errorMsg ∷ Doc
      errorMsg = pretty "the " <> scquotes cmd
                 <> pretty " command associated with "
                 <> pretty atp <> pretty " does not exist"

  checkExecutable cmd errorMsg

  -- To create the ATPs process we follow the ideas used by
  -- @System.Process.proc@.
  (_, outputH, _, atpPH) ← liftIO $
    createProcess CreateProcess
                    { cmdspec            = RawCommand cmd args
                    , cwd                = Nothing
                    , env                = Nothing
                    , std_in             = Inherit
                    , std_out            = CreatePipe
                    , std_err            = Inherit
                    , close_fds          = False
                    , create_group       = True
#if MIN_VERSION_process(1,2,0)
                    , delegate_ctlc      = False
#endif
#if MIN_VERSION_process(1,3,0)
                    , create_new_console = False
                    , detach_console     = False
                    , new_session        = False
#endif
#if MIN_VERSION_process(1,4,0)
                    , child_group        = Nothing
                    , child_user         = Nothing
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
          (putStrLn $ T.pack $ prettyShow msg)
          (E.throwE msg)
    else do
      output ← liftIO $ takeMVar outputMVar
      atpWithVersion ← atpVersion (snd output)
      if fst output
        then do
          reportS "" 1 $ atpWithVersion ++ " proved the conjecture"
          -- See note [Killing the ATPs].
          liftIO $ mapM_
          -- We use @terminateProcess@ on Mac OS X (see Issue #29).
#if defined(darwin_HOST_OS)
                     terminateProcess
#else
                     interruptProcessGroupOf
#endif
                     atpsPH
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

-- Creation date: 2012-06-12

-- Hack. Running for example
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

------------------------------------------------------------------------------
-- Note [Killing the ATPs].

-- Creation date: 2016-09-05

-- After an ATP prove a conjecture, we need to kill the other ATPs
-- that are running. We are using @interruptProcessGroupOf@ instead of
-- @terminateProcess@ because, generally speaking, this function kills
-- faster the running ATPs.
