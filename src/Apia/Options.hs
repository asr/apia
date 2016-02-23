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
           , optDumpTypes
           , optFnConstant
           , optHelp
           , optInputFile
           , optIncludePath
           , optLang
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
           , optWithtptp2X
           , optWithtptp4X
           , optWithVampire
           , optWithZ3
           )
  , printUsage
  , processOptions
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Interaction.Options ( Verbosity )
import Agda.Utils.Impossible    ( Impossible(Impossible), throwImpossible )
import Agda.Utils.List          ( wordsBy )

import qualified Agda.Utils.Trie as Trie ( insert, singleton )

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
  , Lang(FOF, TFF0)
  )

import Apia.Utils.PrettyPrint ( (<>), Doc, Pretty(pretty), squotes )

import Safe ( initDef )

import System.Console.GetOpt
  ( ArgDescr(NoArg, ReqArg)
  , ArgOrder(ReturnInOrder)
  , getOpt
  , OptDescr(Option)
  , usageInfo
  )

import System.Environment ( getProgName )

#include "undefined.h"

-----------------------------------------------------------------------------

-- | Program command-line options.
data Options = Options
  { optATP                             ∷ [String]
  , optCheck                           ∷ Bool
  , optDumpTypes                       ∷ Bool
  , optFnConstant                      ∷ Bool
  , optHelp                            ∷ Bool
  , optIncludePath                     ∷ [FilePath]
  , optInputFile                       ∷ Maybe FilePath
  , optLang                            ∷ Lang
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
  , optWithtptp2X                      ∷ String
  , optWithtptp4X                      ∷ String
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
  , optDumpTypes                       = False
  , optFnConstant                      = False
  , optHelp                            = False
  , optIncludePath                     = []
  , optInputFile                       = Nothing
  , optLang                            = FOF
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
  , optWithtptp2X                      = "tptp2X"
  , optWithtptp4X                      = "tptp4X"
  , optWithVampire                     = "vampire_lin64"
  , optWithZ3                          = "z3"
  }

-- | 'Options' monad.
type MOptions = Options → Either Doc Options

atpOpt ∷ String → MOptions
atpOpt [] _ = Left $
  pretty "option " <> squotes "--atp" <> pretty " requires an argument NAME"
atpOpt name opts = Right opts { optATP = optATP opts ++ [name] }

checkOpt ∷ MOptions
checkOpt opts = Right opts { optCheck = True }

dumpTypesOpt ∷ MOptions
dumpTypesOpt opts = Right opts { optDumpTypes = True }

fnConstantOpt ∷ MOptions
fnConstantOpt opts = Right opts { optFnConstant = True }

helpOpt ∷ MOptions
helpOpt opts = Right opts { optHelp = True }

includePathOpt ∷ FilePath → MOptions
includePathOpt [] _ = Left $
  pretty "option " <> squotes "--include-path"
  <> pretty " requires an argument DIR"
includePathOpt dir opts =
  Right opts { optIncludePath = optIncludePath opts ++ [dir] }

inputFileOpt ∷ FilePath → MOptions
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

langOpt ∷ String → MOptions
langOpt "fof"  opts = Right opts { optLang = FOF }
langOpt "tff0" opts = Right opts { optLang = TFF0 }
langOpt lang   _    = Left $
  pretty "Language " <> pretty lang <> pretty " is not a TPTP language"

noInternalEqualityOpt ∷ MOptions
noInternalEqualityOpt opts = Right opts { optNoInternalEquality = True }

noPredicateConstantsOpt ∷ MOptions
noPredicateConstantsOpt opts = Right opts { optNoPredicateConstants = True }

onlyFilesOpt ∷ MOptions
onlyFilesOpt opts = Right opts { optOnlyFiles = True }

outputDirOpt ∷ FilePath → MOptions
outputDirOpt [] _ = Left $
  pretty "option " <> squotes "--output-dir"
  <> pretty " requires an argument DIR"
outputDirOpt dir opts = Right opts { optOutputDir = dir }

schematicPropositionalFunctionsOpt :: MOptions
schematicPropositionalFunctionsOpt opts =
  Right opts { optSchematicPropositionalFunctions = True }

schematicPropositionalSymbolsOpt :: MOptions
schematicPropositionalSymbolsOpt opts =
  Right opts { optSchematicPropositionalSymbols = True }

snapshotDirOpt ∷ FilePath → MOptions
snapshotDirOpt [] _ = Left $
  pretty "option " <> squotes "--snapshot-dir"
  <> pretty " requires an argument DIR"
snapshotDirOpt dir opts = Right opts { optSnapshotDir = dir }

snapshotNoErrorOpt ∷ MOptions
snapshotNoErrorOpt opts = Right opts { optSnapshotNoError = True
                                     , optSnapshotTest = True
                                     }

snapshotTestOpt ∷ MOptions
snapshotTestOpt opts = Right opts { optSnapshotTest = True }

timeOpt ∷ String → MOptions
timeOpt [] _ = Left $
  pretty "option " <> squotes "--time" <> pretty " requires an argument NUM"
timeOpt secs opts =
  if all isDigit secs
  then Right opts { optTime = read secs }
  else Left $ pretty "option " <> squotes "--time"
              <> pretty " requires a non-negative integer argument"

unprovenNoErrorOpt ∷ MOptions
unprovenNoErrorOpt opts = Right opts { optUnprovenNoError = True }

-- Adapted from @Agda.Interaction.Options.verboseFlag@.
verboseOpt ∷ String → MOptions
verboseOpt [] _ = Left $
 pretty "option " <> squotes "--verbose"
 <> pretty " requires an argument of the form x.y.z:N or N"
verboseOpt str opts =
  Right opts { optVerbose = Trie.insert k n $ optVerbose opts }
  where
  k ∷ [String]
  n ∷ Int
  (k, n) = parseVerbose str

  parseVerbose ∷ String → ([String], Int)
  parseVerbose s =
    case wordsBy (`elem` (":." ∷ String)) s of
      [] → __IMPOSSIBLE__
      ss → let m ∷ Int
               m = read $ last ss
           in  (init ss, m)

versionOpt ∷ MOptions
versionOpt opts = Right opts { optVersion = True }

withCVC4Opt ∷ String → MOptions
withCVC4Opt [] _ = Left $
  pretty "option " <> squotes "--with-cvc4"
  <> pretty " requires an argument PATH"
withCVC4Opt name opts = Right opts { optWithCVC4 = name }

withEOpt ∷ String → MOptions
withEOpt [] _ = Left $
  pretty "option " <> squotes "--with-e"
  <> pretty " requires an argument PATH"
withEOpt name opts = Right opts { optWithE = name }

withEquinoxOpt ∷ String → MOptions
withEquinoxOpt [] _ = Left $
  pretty "option " <> squotes "--with-equinox"
  <> pretty " requires an argument PATH"
withEquinoxOpt name opts = Right opts { optWithEquinox = name }

withIleanCoPOpt ∷ String → MOptions
withIleanCoPOpt [] _ = Left $
  pretty "option " <> squotes "--with-ileancop"
  <> pretty " requires an argument PATH"
withIleanCoPOpt name opts = Right opts { optWithIleanCoP = name }

withMetisOpt ∷ String → MOptions
withMetisOpt [] _ = Left $
  pretty "option " <> squotes "--with-metis"
  <> pretty " requires an argument PATH"
withMetisOpt name opts = Right opts { optWithMetis = name }

withSPASSOpt ∷ String → MOptions
withSPASSOpt [] _ = Left $
  pretty "option " <> squotes "--with-spass"
  <> pretty " requires an argument PATH"
withSPASSOpt name opts = Right opts { optWithSPASS = name }

withtptp2XOpt ∷ String → MOptions
withtptp2XOpt [] _ = Left $
  pretty "option " <> squotes "--with-tptp2X"
  <> pretty " requires an argument PATH"
withtptp2XOpt name opts = Right opts { optWithtptp2X = name }

withtptp4XOpt ∷ String → MOptions
withtptp4XOpt [] _ = Left $
  pretty "option " <> squotes "--with-tptp4X"
  <> pretty " requires an argument PATH"
withtptp4XOpt name opts = Right opts { optWithtptp4X = name }

withVampireOpt ∷ String → MOptions
withVampireOpt [] _  = Left $
  pretty "option " <> squotes "--with-vampire"
  <> pretty " requires an argument PATH"
withVampireOpt name opts = Right opts { optWithVampire = name }

withZ3Opt ∷ String → MOptions
withZ3Opt []   _    = Left $
  pretty "option " <> squotes "--with-z3"
  <> pretty " requires an argument PATH"
withZ3Opt name opts = Right opts { optWithZ3 = name }

-- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "NAME") $
               "Set the ATP (cvc4, e, equinox, ileancop, metis, spass, vampire or z3)\n"
               ++ "(default: e, equinox and vampire)"
  , Option []  ["check"] (NoArg checkOpt) $
               "Check the syntax of the generated TPTP files using the\n"
               ++ "tptp4X program from the TPTP library"
  , Option []  ["dump-types"] (NoArg dumpTypesOpt)
               "Dump Agda types information to stdout"
  , Option []  ["function-constant"] (NoArg fnConstantOpt) $
               "Use a hard-coded binary function symbol for the translation\n"
               ++ "of functions (required for handling currying)"
  , Option []  ["help"] (NoArg helpOpt)
               "Show this help"
  , Option "i" ["include-path"] (ReqArg includePathOpt "DIR")
               "Look for imports in DIR"
  , Option "L" ["lang"] (ReqArg langOpt "LANG") $
               "TPTP output language (fof or tff0)\n"
               ++ "(default: fof)"
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
  , Option []  ["with-tptp2X"] (ReqArg withtptp2XOpt "PATH")
               "Give the path to tptp2X"
  , Option []  ["with-tptp4X"] (ReqArg withtptp4XOpt "PATH")
               "Give the path to tptp4X"
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
    (_, _, errs) → Left $ pretty $ initDef (__IMPOSSIBLE__) $ init $ unlines errs

-- | Processing the command-line 'Options'.
processOptions ∷ [String] → Either Doc Options
processOptions argv = processOptionsHelper argv inputFileOpt defaultOptions
