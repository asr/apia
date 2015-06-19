-----------------------------------------------------------------------------
-- |
-- Module      : Apia.Options
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Process the command-line arguments.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Options
  ( defaultOptions
  , options
  , MOptions  -- Required by Haddock.
  , Options( Options --Improve Haddock information.
           , optATP
           , optCheck
           , optDumpAgdai
           , optDumpQNames
           , optHelp
           , optInputFile
           , optIncludePath
           , optOnlyFiles
           , optOutputDir
           , optSchematicFunctions
           , optSchematicPropositionalFunctions
           , optSchematicPropositionalSymbols
           , optSnapshotDir
           , optSnapshotNoError
           , optSnapshotTest
           , optTime
           , optUnprovenNoError
           , optVampireExec
           , optVerbose
           , optVersion
           , optWithFnConsts
           , optWithoutPConsts
           )
  , printUsage
  , processOptions
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Char ( isDigit )
import Data.List ( foldl' )

import Safe ( initDef )

import System.Console.GetOpt
  ( ArgDescr(NoArg, ReqArg)
  , ArgOrder(ReturnInOrder)
  , getOpt
  , OptDescr(Option)
  , usageInfo
  )

import System.Environment ( getProgName )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Interaction.Options ( Verbosity )
import Agda.Utils.Impossible    ( Impossible(Impossible), throwImpossible )
import Agda.Utils.List          ( wordsBy )

import qualified Agda.Utils.Trie as Trie ( insert, singleton )

------------------------------------------------------------------------------
-- Apia imports

#include "undefined.h"

-----------------------------------------------------------------------------

-- | Program command-line options.
data Options = Options
  { optATP                             ∷ [String]
  , optCheck                           ∷ Bool
  , optDumpAgdai                       ∷ Bool
  , optDumpQNames                      ∷ Bool
  , optHelp                            ∷ Bool
  , optIncludePath                     ∷ [FilePath]
  , optInputFile                       ∷ Maybe FilePath
  , optOnlyFiles                       ∷ Bool
  , optOutputDir                       ∷ FilePath
  , optSchematicFunctions              ∷ Bool
  , optSchematicPropositionalFunctions ∷ Bool
  , optSchematicPropositionalSymbols   ∷ Bool
  , optSnapshotDir                     ∷ FilePath
  , optSnapshotNoError                 ∷ Bool
  , optSnapshotTest                    ∷ Bool
  , optTime                            ∷ Int
  , optUnprovenNoError                 ∷ Bool
  , optVampireExec                     ∷ String
  , optVerbose                         ∷ Verbosity
  , optVersion                         ∷ Bool
  , optWithFnConsts                    ∷ Bool
  , optWithoutPConsts                  ∷ Bool
  }

-- N.B. The default ATPs are handled by @ATP.callATPs@.
--
-- | Default options use by the program.
defaultOptions ∷ Options
defaultOptions = Options
  { optATP                             = []
  , optCheck                           = False
  , optDumpAgdai                       = False
  , optDumpQNames                      = False
  , optHelp                            = False
  , optIncludePath                     = []
  , optInputFile                       = Nothing
  , optOnlyFiles                       = False
  , optOutputDir                       = "/tmp"
  , optSchematicFunctions              = False
  , optSchematicPropositionalFunctions = False
  , optSchematicPropositionalSymbols   = False
  , optSnapshotDir                     = "snapshot"
  , optSnapshotNoError                 = False
  , optSnapshotTest                    = False
  , optTime                            = 240
  , optUnprovenNoError                 = False
  , optVampireExec                     = "vampire_lin64"
  , optVerbose                         = Trie.singleton [] 1
  , optVersion                         = False
  , optWithFnConsts                    = False
  , optWithoutPConsts                  = False
  }

-- | 'Options' monad.
type MOptions = Options → Either String Options

atpOpt ∷ String → MOptions
atpOpt []   _    = Left "Option `--atp' requires an argument NAME"
atpOpt name opts = Right opts { optATP = optATP opts ++ [name] }

checkOpt ∷ MOptions
checkOpt opts = Right opts { optCheck = True }

dumpAgdaiOpt ∷ MOptions
dumpAgdaiOpt opts = Right opts { optDumpAgdai = True }

dumpQNamesOpt ∷ MOptions
dumpQNamesOpt opts = Right opts { optDumpQNames = True }

helpOpt ∷ MOptions
helpOpt opts = Right opts { optHelp = True }

includePathOpt ∷ FilePath → MOptions
includePathOpt [] _ = error "Option `--include-path' requires an argument DIR"
includePathOpt dir opts =
  Right opts { optIncludePath = optIncludePath opts ++ [dir] }

inputFileOpt ∷ FilePath → MOptions
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left "Only one input file allowed"

onlyFilesOpt ∷ MOptions
onlyFilesOpt opts = Right opts { optOnlyFiles = True }

outputDirOpt ∷ FilePath → MOptions
outputDirOpt []  _    = Left "Option `--output-dir' requires an argument DIR"
outputDirOpt dir opts = Right opts { optOutputDir = dir }

schematicFunctionsOpt :: MOptions
schematicFunctionsOpt opts = Right opts { optSchematicFunctions = True }

schematicPropositionalFunctionsOpt :: MOptions
schematicPropositionalFunctionsOpt opts =
  Right opts { optSchematicPropositionalFunctions = True }

schematicPropositionalSymbolsOpt :: MOptions
schematicPropositionalSymbolsOpt opts =
  Right opts { optSchematicPropositionalSymbols = True }

snapshotDirOpt ∷ FilePath → MOptions
snapshotDirOpt []  _    = Left "Option `--snapshot-dir' requires an argument DIR"
snapshotDirOpt dir opts = Right opts { optSnapshotDir = dir }

snapshotNoErrorOpt ∷ MOptions
snapshotNoErrorOpt opts = Right opts { optSnapshotNoError = True
                                     , optSnapshotTest = True
                                     }

snapshotTestOpt ∷ MOptions
snapshotTestOpt opts = Right opts { optSnapshotTest = True }

timeOpt ∷ String → MOptions
timeOpt []   _    = Left "Option `--time' requires an argument NUM"
timeOpt secs opts =
  if all isDigit secs
  then Right opts { optTime = read secs }
  else Left "Option `--time' requires a non-negative integer argument"

unprovenNoErrorOpt ∷ MOptions
unprovenNoErrorOpt opts = Right opts { optUnprovenNoError = True }

vampireExecOpt ∷ String → MOptions
vampireExecOpt []   _    = Left "Option `--vampire-exec' requires an argument COMMAND"
vampireExecOpt name opts = Right opts { optVampireExec = name }

-- Adapted from @Agda.Interaction.Options.verboseFlag@.
verboseOpt ∷ String → MOptions
verboseOpt [] _ = Left "Option `--verbose' requires an argument of the form x.y.z:N or N"
verboseOpt str opts =
  Right opts { optVerbose = Trie.insert k n $ optVerbose opts }
  where
  k ∷ [String]
  n ∷ Int
  (k, n) = parseVerbose str

  parseVerbose ∷ String → ([String], Int)
  parseVerbose s =
    case wordsBy (`elem` ":.") s of
      [] → __IMPOSSIBLE__
      ss → let m ∷ Int
               m = read $ last ss
           in  (init ss, m)

versionOpt ∷ MOptions
versionOpt opts = Right opts { optVersion = True }

withFnConstsOpt ∷ MOptions
withFnConstsOpt opts = Right opts { optWithFnConsts = True }

withoutPConstsOpt ∷ MOptions
withoutPConstsOpt opts = Right opts { optWithoutPConsts = True }

-- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "NAME") $
               "Set the ATP (e, equinox, ileancop, metis, spass, vampire)\n"
               ++ "(default: e, equinox, and vampire)."
  , Option []  ["check"] (NoArg checkOpt) $
               "Check the syntax of the generated TPTP files using the\n"
               ++ "tptp4X program from the TPTP library."
  , Option []  ["dump-agdai"] (NoArg dumpAgdaiOpt)
               "Dump the Agda interface file to stdout."
  , Option []  ["dump-qnames"] (NoArg dumpQNamesOpt)
               "Dump Agda QNames information to stdout."
  , Option []  ["help"] (NoArg helpOpt)
               "Show this help."
  , Option "i" ["include-path"] (ReqArg includePathOpt "DIR")
               "Look for imports in DIR."
  , Option []  ["only-files"] (NoArg onlyFilesOpt)
               "Do not call the ATPs, only to create the TPTP files."
  , Option []  ["output-dir"] (ReqArg outputDirOpt "DIR")
               "Directory in which the TPTP files are placed (default: /tmp)."
  , Option []  ["schematic-propositional-functions"]
               (NoArg schematicPropositionalFunctionsOpt)
               "Enable translation of universal quantified FOL propositional functions"
  , Option []  ["schematic-propositional-symbols"]
               (NoArg schematicPropositionalSymbolsOpt)
               "Enable translation of universal quantified FOL propositional symbols"
  , Option []  ["snapshot-dir"] (ReqArg snapshotDirOpt "DIR") $
               "Directory where is the snapshot of the TPTP files\n"
               ++ "(default: snapshot)."
  , Option []  ["snapshot-no-error"] (NoArg snapshotNoErrorOpt) $
               "A difference in the snapshot-test does not generate an error\n"
               ++ "(implies --snapshot-test)."
  , Option []  ["snapshot-test"] (NoArg snapshotTestOpt)
               "Compare the generated TPTP files against a snapshot of them."
  , Option []  ["time"] (ReqArg timeOpt "NUM")
               "Set timeout for the ATPs in seconds (default: 240)."
  , Option []  ["unproven-conjecture-no-error"] (NoArg unprovenNoErrorOpt)
               "An unproven TPTP conjecture does not generate an error."
  , Option []  ["vampire-exec"] (ReqArg vampireExecOpt "COMMAND")
               "Set the Vampire executable (default: vampire_lin64)."
  , Option "v" ["verbose"] (ReqArg verboseOpt "N")
               "Set verbosity level to N."
  , Option []  ["version"] (NoArg versionOpt)
               "Show version number."
  , Option []  ["with-function-constants"] (NoArg withFnConstsOpt) $
               "Use a hard-coded binary function symbol for the translation\n"
               ++ "of functions (required for handling currying)."
  , Option []  ["without-predicate-constants"] (NoArg withoutPConstsOpt) $
               "Do not use hard-coded (n+1)-ary predicate symbols for the\n"
               ++ "translation of n-ary predicates."
  ]

usageHeader ∷ String → String
usageHeader prgName = "Usage: " ++ prgName ++ " [OPTIONS] FILE\n"

-- | Print usage information.
printUsage ∷ IO ()
printUsage = do
  progName ← getProgName
  putStrLn $ usageInfo (usageHeader progName) options

processOptionsHelper ∷ [String] → (FilePath → MOptions) → MOptions
processOptionsHelper argv f defaults =
  case getOpt (ReturnInOrder f) options argv of
    (o, _, [])   → foldl' (>>=) (return defaults) o
    (_, _, errs) → Left $ initDef (__IMPOSSIBLE__) $ init $ unlines errs

-- | Processing the command-line 'Options'.
processOptions ∷ [String] → Either String Options
processOptions argv = processOptionsHelper argv inputFileOpt defaultOptions
