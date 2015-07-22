-----------------------------------------------------------------------------
-- |
-- Module      : Options
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

module Options
  ( defaultOptions
  , options
  , MOptions  -- Required by Haddock.
  , Options( Options --Improve Haddock information.
           , optATP
           , optCheck
           , optDumpAgdai
           , optDumpQNames
           , optFnConstant
           , optHelp
           , optInputFile
           , optIncludePath
           , optNoInternalEquality
           , optNoPredicateConstants
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
           , optVerbose
           , optVersion
           , optWithCVC4
           , optWithE
           , optWithEquinox
           , optWithIleanCoP
           , optWithMetis
           , optWithSPASS
           , optWithVampire
           , optWithZ3
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

import Common
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

#include "undefined.h"

-----------------------------------------------------------------------------

-- | Program command-line options.
data Options = Options
  { optATP                             ∷ [String]
  , optCheck                           ∷ Bool
  , optDumpAgdai                       ∷ Bool
  , optDumpQNames                      ∷ Bool
  , optFnConstant                      ∷ Bool
  , optHelp                            ∷ Bool
  , optIncludePath                     ∷ [FilePath]
  , optInputFile                       ∷ Maybe FilePath
  , optNoInternalEquality              ∷ Bool
  , optNoPredicateConstants            ∷ Bool
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
  , optVerbose                         ∷ Verbosity
  , optVersion                         ∷ Bool
  , optWithCVC4                        ∷ String
  , optWithE                           ∷ String
  , optWithEquinox                     ∷ String
  , optWithIleanCoP                    ∷ String
  , optWithMetis                       ∷ String
  , optWithSPASS                       ∷ String
  , optWithVampire                     ∷ String
  , optWithZ3                          ∷ String
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
  , optFnConstant                      = False
  , optHelp                            = False
  , optIncludePath                     = []
  , optInputFile                       = Nothing
  , optNoInternalEquality              = False
  , optNoPredicateConstants            = False
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
  , optVerbose                         = Trie.singleton [] 1
  , optVersion                         = False
  , optWithCVC4                        = "cvc4"
  , optWithE                           = "eprover"
  , optWithEquinox                     = "equinox"
  , optWithIleanCoP                    = "ileancop.sh"
  , optWithMetis                       = "metis"
  , optWithSPASS                       = "SPASS"
  , optWithVampire                     = "vampire_lin64"
  , optWithZ3                          = "z3"
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

fnConstantOpt ∷ MOptions
fnConstantOpt opts = Right opts { optFnConstant = True }

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

noInternalEqualityOpt ∷ MOptions
noInternalEqualityOpt opts = Right opts { optNoInternalEquality = True }

noPredicateConstantsOpt ∷ MOptions
noPredicateConstantsOpt opts = Right opts { optNoPredicateConstants = True }

onlyFilesOpt ∷ MOptions
onlyFilesOpt opts = Right opts { optOnlyFiles = True }

outputDirOpt ∷ FilePath → MOptions
outputDirOpt []  _    = Left "Option `--output-dir' requires an argument DIR"
outputDirOpt dir opts = Right opts { optOutputDir = dir }

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

withCVC4Opt ∷ String → MOptions
withCVC4Opt []   _    = Left "Option `--with-cvc4' requires an argument PATH"
withCVC4Opt name opts = Right opts { optWithCVC4 = name }

withEOpt ∷ String → MOptions
withEOpt []   _    = Left "Option `--with-e' requires an argument PATH"
withEOpt name opts = Right opts { optWithE = name }

withEquinoxOpt ∷ String → MOptions
withEquinoxOpt []   _    = Left "Option `--with-equinox' requires an argument PATH"
withEquinoxOpt name opts = Right opts { optWithEquinox = name }

withIleanCoPOpt ∷ String → MOptions
withIleanCoPOpt []   _    = Left "Option `--with-ileancop' requires an argument PATH"
withIleanCoPOpt name opts = Right opts { optWithIleanCoP = name }

withMetisOpt ∷ String → MOptions
withMetisOpt []   _    = Left "Option `--with-metis' requires an argument PATH"
withMetisOpt name opts = Right opts { optWithMetis = name }

withSPASSOpt ∷ String → MOptions
withSPASSOpt []   _    = Left "Option `--with-spass' requires an argument PATH"
withSPASSOpt name opts = Right opts { optWithSPASS = name }

withVampireOpt ∷ String → MOptions
withVampireOpt []   _    = Left "Option `--with-vampire' requires an argument PATH"
withVampireOpt name opts = Right opts { optWithVampire = name }

withZ3Opt ∷ String → MOptions
withZ3Opt []   _    = Left "Option `--with-z3' requires an argument PATH"
withZ3Opt name opts = Right opts { optWithZ3 = name }

-- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "NAME") $
               "Set the ATP (cvc4, e, equinox, ileancop, metis, spass, vampire, z3)\n"
               ++ "(default: e, equinox, and vampire)"
  , Option []  ["check"] (NoArg checkOpt) $
               "Check the syntax of the generated TPTP files using the\n"
               ++ "tptp4X program from the TPTP library"
  , Option []  ["dump-agdai"] (NoArg dumpAgdaiOpt)
               "Dump the Agda interface file to stdout"
  , Option []  ["dump-qnames"] (NoArg dumpQNamesOpt)
               "Dump Agda QNames information to stdout"
  , Option []  ["function-constant"] (NoArg fnConstantOpt) $
               "Use a hard-coded binary function symbol for the translation\n"
               ++ "of functions (required for handling currying)"
  , Option []  ["help"] (NoArg helpOpt)
               "Show this help"
  , Option "i" ["include-path"] (ReqArg includePathOpt "DIR")
               "Look for imports in DIR"
  , Option []  ["no-internal-equality"] (NoArg noInternalEqualityOpt)
               "Do not translate _≡_ to the ATPs equality"
  , Option []  ["no-predicate-constants"] (NoArg noPredicateConstantsOpt) $
               "Do not use hard-coded (n+1)-ary predicate symbols for the\n"
               ++ "translation of n-ary predicates"
  , Option []  ["only-files"] (NoArg onlyFilesOpt)
               "Do not call the ATPs, only to create the TPTP files"
  , Option []  ["output-dir"] (ReqArg outputDirOpt "DIR")
               "Directory in which the TPTP files are placed (default: /tmp)"
  , Option []  ["schematic-propositional-functions"]
               (NoArg schematicPropositionalFunctionsOpt)
               "Enable translation of universal quantified FOL propositional functions"
  , Option []  ["schematic-propositional-symbols"]
               (NoArg schematicPropositionalSymbolsOpt)
               "Enable translation of universal quantified FOL propositional symbols"
  , Option []  ["snapshot-dir"] (ReqArg snapshotDirOpt "DIR") $
               "Directory where is the snapshot of the TPTP files\n"
               ++ "(default: snapshot)"
  , Option []  ["snapshot-no-error"] (NoArg snapshotNoErrorOpt) $
               "A difference in the snapshot-test does not generate an error\n"
               ++ "(implies --snapshot-test)"
  , Option []  ["snapshot-test"] (NoArg snapshotTestOpt)
               "Compare the generated TPTP files against a snapshot of them"
  , Option []  ["time"] (ReqArg timeOpt "NUM")
               "Set timeout for the ATPs in seconds (default: 240)"
  , Option []  ["unproven-conjecture-no-error"] (NoArg unprovenNoErrorOpt)
               "An unproven TPTP conjecture does not generate an error"
  , Option "v" ["verbose"] (ReqArg verboseOpt "N")
               "Set verbosity level to N"
  , Option []  ["version"] (NoArg versionOpt)
               "Show version number"
  , Option []  ["with-cvc4"] (ReqArg withCVC4Opt "PATH") $
               "Give the path to " ++ show CVC4
  , Option []  ["with-e"] (ReqArg withEOpt "PATH") $
               "Give the path to " ++ show E
  , Option []  ["with-equinox"] (ReqArg withEquinoxOpt "PATH") $
               "Give the path to " ++ show Equinox
  , Option []  ["with-ileancop"] (ReqArg withIleanCoPOpt "PATH") $
               "Give the path to " ++ show IleanCoP
  , Option []  ["with-metis"] (ReqArg withMetisOpt "PATH") $
               "Give the path to " ++ show Metis
  , Option []  ["with-spass"] (ReqArg withSPASSOpt "PATH") $
               "Give the path to " ++ show SPASS
  , Option []  ["with-vampire"] (ReqArg withVampireOpt "PATH") $
               "Give the path to " ++ show Vampire
  , Option []  ["with-z3"] (ReqArg withZ3Opt "PATH") $
               "Give the path to " ++ show Z3
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
