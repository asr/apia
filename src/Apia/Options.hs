
-- | Process the command-line arguments.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Options
  ( extractATPs
  , ManagerATP(..)
  , options
  , OM  -- Required by Haddock.
  , Options( Options --Improve Haddock information.
           , optATP
           , optCheck
           , optDumpTypes
           , optFnConstant
           , optHelp
           , optIncludePath
           , optInputFile
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
           , optWithOnlineATPs
           , optWithSPASS
           , optWithtptp4X
           , optWithVampire
           , optWithZ3
           )
  , printUsage
  , processOptions
  , verboseOpt
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Interaction.Options ( Verbosity )
import Agda.Utils.Impossible    ( Impossible(Impossible), throwImpossible )
import Agda.Utils.List          ( wordsBy )

import qualified Agda.Utils.Trie as Trie ( insert )

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
  , Lang(SMT2, TPTP)
  )

import Apia.Utils.PrettyPrint ( (<>), Doc, Pretty(pretty), scquotes )
import qualified Data.Text as T ( pack )

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

-- | A data type to deal with the option optATP

data ManagerATP a = DefaultATPs a | CommandATPs a

extractATPs ∷ ManagerATP a → a
extractATPs (DefaultATPs val) = val
extractATPs (CommandATPs val) = val


-- | Program command-line options.
data Options = Options
  { optATP                             ∷ ManagerATP [String]
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
  , optWithOnlineATPs                  ∷ String
  , optWithSPASS                       ∷ String
  , optWithtptp4X                      ∷ String
  , optWithVampire                     ∷ String
  , optWithZ3                          ∷ String
  }

-- | 'Options' monad.
type OM = Options → Either Doc Options

atpOpt ∷ String → OM
atpOpt [] _ = Left $
  pretty "option " <> scquotes "--atp" <> pretty " requires an argument NAME"
atpOpt name opts = Right opts { optATP = CommandATPs atps }
  where
    atps ∷ [String]
    atps = case optATP opts of
      CommandATPs old → nub $ old ++ [name]
      DefaultATPs _   → [name]

checkOpt ∷ OM
checkOpt opts = Right opts { optCheck = True }

noCheckOpt ∷ OM
noCheckOpt opts = Right opts { optCheck = False }

dumpTypesOpt ∷ OM
dumpTypesOpt opts = Right opts { optDumpTypes = True }

fnConstantOpt ∷ OM
fnConstantOpt opts = Right opts { optFnConstant = True }

helpOpt ∷ OM
helpOpt opts = Right opts { optHelp = True }

includePathOpt ∷ FilePath → OM
includePathOpt [] _ = Left $
  pretty "option " <> scquotes "--include-path"
  <> pretty " requires an argument DIR"
includePathOpt dir opts =
  Right opts { optIncludePath = optIncludePath opts ++ [dir] }

inputFileOpt ∷ FilePath → OM
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

langOpt ∷ String → OM
langOpt "tptp" opts = Right opts { optLang = TPTP }
langOpt "smt2" opts = Right opts { optLang = SMT2 }
langOpt lang      _ = Left $
  pretty "Language " <> pretty lang <> pretty " is not a valid output language"

noInternalEqualityOpt ∷ OM
noInternalEqualityOpt opts = Right opts { optNoInternalEquality = True }

noPredicateConstantsOpt ∷ OM
noPredicateConstantsOpt opts = Right opts { optNoPredicateConstants = True }

onlyFilesOpt ∷ OM
onlyFilesOpt opts = Right opts { optOnlyFiles = True }

outputDirOpt ∷ FilePath → OM
outputDirOpt [] _ = Left $
  pretty "option " <> scquotes "--output-dir"
  <> pretty " requires an argument DIR"
outputDirOpt dir opts = Right opts { optOutputDir = dir }

schematicPropositionalFunctionsOpt :: OM
schematicPropositionalFunctionsOpt opts =
  Right opts { optSchematicPropositionalFunctions = True }

schematicPropositionalSymbolsOpt :: OM
schematicPropositionalSymbolsOpt opts =
  Right opts { optSchematicPropositionalSymbols = True }

snapshotDirOpt ∷ FilePath → OM
snapshotDirOpt [] _ = Left $
  pretty "option " <> scquotes "--snapshot-dir"
  <> pretty " requires an argument DIR"
snapshotDirOpt dir opts = Right opts { optSnapshotDir = dir }

snapshotNoErrorOpt ∷ OM
snapshotNoErrorOpt opts = Right opts { optSnapshotNoError = True
                                     , optSnapshotTest = True
                                     }

snapshotTestOpt ∷ OM
snapshotTestOpt opts = Right opts { optSnapshotTest = True }

timeOpt ∷ String → OM
timeOpt [] _ = Left $
  pretty "option " <> scquotes "--time" <> pretty " requires an argument NUM"
timeOpt secs opts =
  if all isDigit secs
  then Right opts { optTime = read secs }
  else Left $ pretty "option " <> scquotes "--time"
              <> pretty " requires a non-negative integer argument"

unprovenNoErrorOpt ∷ OM
unprovenNoErrorOpt opts = Right opts { optUnprovenNoError = True }

-- Adapted from @Agda.Interaction.Options.verboseFlag@.
verboseOpt ∷ String → OM
verboseOpt [] _ = Left $
 pretty "option " <> scquotes "--verbose"
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

versionOpt ∷ OM
versionOpt opts = Right opts { optVersion = True }

withCVC4Opt ∷ String → OM
withCVC4Opt [] _ = Left $
  pretty "option " <> scquotes "--with-cvc4"
  <> pretty " requires an argument PATH"
withCVC4Opt name opts = Right opts { optWithCVC4 = name }

withEOpt ∷ String → OM
withEOpt [] _ = Left $
  pretty "option " <> scquotes "--with-e"
  <> pretty " requires an argument PATH"
withEOpt name opts = Right opts { optWithE = name }

withEquinoxOpt ∷ String → OM
withEquinoxOpt [] _ = Left $
  pretty "option " <> scquotes "--with-equinox"
  <> pretty " requires an argument PATH"
withEquinoxOpt name opts = Right opts { optWithEquinox = name }

withIleanCoPOpt ∷ String → OM
withIleanCoPOpt [] _ = Left $
  pretty "option " <> scquotes "--with-ileancop"
  <> pretty " requires an argument PATH"
withIleanCoPOpt name opts = Right opts { optWithIleanCoP = name }

withMetisOpt ∷ String → OM
withMetisOpt [] _ = Left $
  pretty "option " <> scquotes "--with-metis"
  <> pretty " requires an argument PATH"
withMetisOpt name opts = Right opts { optWithMetis = name }

withOnlineATPsOpt ∷ String → OM
withOnlineATPsOpt [] _ = Left $
  pretty "option " <> scquotes "--with-onlineatps"
  <> pretty " requires an argument PATH"
withOnlineATPsOpt name opts = Right opts { optWithOnlineATPs = name }

withSPASSOpt ∷ String → OM
withSPASSOpt [] _ = Left $
  pretty "option " <> scquotes "--with-spass"
  <> pretty " requires an argument PATH"
withSPASSOpt name opts = Right opts { optWithSPASS = name }

withtptp4XOpt ∷ String → OM
withtptp4XOpt [] _ = Left $
  pretty "option " <> scquotes "--with-tptp4X"
  <> pretty " requires an argument PATH"
withtptp4XOpt name opts = Right opts { optWithtptp4X = name }

withVampireOpt ∷ String → OM
withVampireOpt [] _  = Left $
  pretty "option " <> scquotes "--with-vampire"
  <> pretty " requires an argument PATH"
withVampireOpt name opts = Right opts { optWithVampire = name }

withZ3Opt ∷ String → OM
withZ3Opt []   _    = Left $
  pretty "option " <> scquotes "--with-z3"
  <> pretty " requires an argument PATH"
withZ3Opt name opts = Right opts { optWithZ3 = name }

-- | Description of the command-line 'Options'.
options ∷ [OptDescr OM]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "[online-]NAME") $
               "Set the ATP\n"
               ++ "(offline ATPs: cvc4, e, equinox, ileancop, metis, spass, vampire or z3)\n"
               ++ "(online ATPs: run the ‘onlineatps --list-atps’ command)"
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
               "TPTP or SMT-LIB v2 output language (tptp or smt2)\n"
               ++ "(default: tptp)"
  , Option []  ["no-check"] (NoArg noCheckOpt) $
               "Do not check the syntax of the generated TPTP files using the\n"
               ++ "tptp4X program from the TPTP library"
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
  , Option []  ["with-onlineatps"] (ReqArg withOnlineATPsOpt "PATH")
               "Give the path to OnlineATPs"
  , Option []  ["with-spass"] (ReqArg withSPASSOpt "PATH") $
               "Give the path to " ++ show SPASS
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
  putStrLn . T.pack $ usageInfo (usageHeader progName) options

processOptionsHelper ∷ [String] → (FilePath → OM) → OM
processOptionsHelper argv f defaults =
  case getOpt (ReturnInOrder f) options argv of
    (o, _, [])   → foldl' (>>=) (return defaults) o
    (_, _, errs) → Left $ pretty $ initDef (__IMPOSSIBLE__) $ init $ unlines errs

-- | Processing the command-line 'Options'.
processOptions ∷ [String] → Options → Either Doc Options
processOptions argv = processOptionsHelper argv inputFileOpt
