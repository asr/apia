
-- | Get the default values arguments.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.Defaults
  ( apiaNameFile
  , getDefaults
  )
  where

------------------------------------------------------------------------------

import           Apia.Utils.Yaml

import Apia.Options
  ( ManagerATP(DefaultATPs)
  , Options(..)
  , verboseOpt
  )

import Apia.Prelude hiding(lookup)
import           Apia.Common           (Lang (SMT2, TPTP))
import Agda.Utils.Trie as Trie
import qualified Data.Text             as T

import           Control.Monad         (filterM)
import           Paths_apia            (getDataFileName)
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        getHomeDirectory)
import           System.FilePath.Posix ((</>))

------------------------------------------------------------------------------


-- | Default options use by the program.
defaultOptions ∷ Options
defaultOptions = Options
  { optATP                             = DefaultATPs []
  , optCheck                           = False
  , optDumpTypes                       = False
  , optFnConstant                      = False
  , optHelp                            = False
  , optIncludePath                     = []
  , optInputFile                       = Nothing
  , optLang                            = TPTP
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
  , optWithOnlineATPs                  = "onlineatps"
  , optWithSPASS                       = "SPASS"
  , optWithtptp4X                      = "tptp4X"
  , optWithVampire                     = "vampire_lin64"
  , optWithZ3                          = "z3"
}

apiaNameFile ∷ FilePath
apiaNameFile = ".apia"

apiaTemplate ∷ FilePath
apiaTemplate = "apia.yml"

combineConfigs ∷ [Object] → Parser Options
combineConfigs configs  =  do
  optATP
     ← configs .@. "atp"
  optCheck
     ← configs .@. "check"
  optDumpTypes
     ← configs .@. "dump-types"
  optFnConstant
     ← configs .@. "function-constant"
  optIncludePath
     ← configs .@. "include-path"
  optLang
     ← configs .@. "lang"
  optNoInternalEquality
     ← configs .@. "no-internal-equality"
  optNoPredicateConstants
     ← configs .@. "no-predicate-constants"
  optOnlyFiles
     ← configs .@. "only-files"
  optOutputDir
     ← configs .@. "output-dir"
  optSchematicFunctions
     ← configs .@. "schematic-functions"
  optSchematicPropositionalFunctions
     ← configs .@. "schematic-propositional-functions"
  optSchematicPropositionaSymbol
     ← configs .@. "schematic-propositional-symbols"
  optSnapshotDir
     ← configs .@. "snapshot-dir"
  optSnapshotNoError
     ← configs .@. "snapshot-no-error"
  optSnapshotTest
     ← configs .@. "snapshot-test"
  optTime
     ← configs .@. "time"
  optUnprovenNoError
     ← configs .@. "unproven-conjecture-no-error"
  optVerbose
     ← configs .@. "verbose"
  optWithCVC4
     ← configs .@. "with-cvc4"
  optWithE
     ← configs .@. "with-e"
  optWithEquinox
     ← configs .@. "with-equinox"
  optWithIleanCoP
     ← configs .@. "with-ileancop"
  optWithMetis
     ← configs .@. "with-metis"
  optWithOnlineATPs
     ← configs .@. "with-onlineatps"
  optWithSPASS
     ← configs .@. "with-spass"
  optWithtptp4X
     ← configs .@. "with-tptp4X"
  optWithVampire
     ← configs .@. "with-vampire"
  optWithZ3
     ← configs .@. "with-z3"
  return Options{..}

-- | Get the default values for the command-line 'Options'.
getDefaults ∷ IO Options
getDefaults = do
  paths ← sequence [getCurrentDirectory, getHomeDirectory]
  userFiles ← filterM doesFileExist $ map (</>apiaNameFile) paths
  defaultConfig ← getDataFileName apiaTemplate

  let allFiles ∷ [FilePath]
      allFiles = userFiles ++ [defaultConfig]

  loaded ∷ [Maybe Object] ← mapM loadYAML allFiles

  let combined ∷ Either String Options
      combined = parseEither combineConfigs $ catMaybes loaded

  case combined of
    Left msg  → do
      putStrLn $ T.pack msg
      return defaultOptions
    Right o   → return o

