
-- | Get the default values arguments.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Defaults
  ( defaultOptions
  , getDefaults
  ) where

import Apia.Prelude

import Apia.Common  ( Lang (SMT2, TPTP) )
import Apia.Options ( ManagerATP(DefaultATPs), Options(..), verboseOpt )

import Agda.Utils.Trie as Trie

import Control.DeepSeq ( NFData(rnf) )

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as T

import Data.Yaml ( FromJSON (parseJSON), Object, parseMaybe )
import qualified Data.Yaml.Include as YamlInclude

import Paths_apia     ( getDataFileName )
import Prelude hiding ( lookup )

import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  )

import System.FilePath.Posix ((</>))

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
  , optNoConfigFile                    = False
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

type Key = T.Text

newtype Config = Config Object
               deriving (Eq, Show)

instance NFData Config where
  rnf (Config o) = rnf o `seq` ()

lookConfig ∷ FromJSON b ⇒ Key → Config → Maybe b
lookConfig k (Config o) = HashMap.lookup k o >>= parseMaybe parseJSON

setATP ∷ Config → Options → Options
setATP config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "atp" config of
      Just val → opts { optATP = DefaultATPs val }
      _        → opts

setCheck ∷ Config → Options → Options
setCheck config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "check" config of
      Just val → opts { optCheck = val }
      _        → opts

setDumpTypes ∷ Config → Options → Options
setDumpTypes config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "dump-types" config of
      Just val → opts { optDumpTypes = val }
      _        → opts

setFnConstant ∷ Config → Options → Options
setFnConstant config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "function-constant" config of
      Just val → opts { optFnConstant = val }
      _        → opts

setIncludePath ∷ Config → Options → Options
setIncludePath config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "include-path" config of
      Just val → opts { optIncludePath = val }
      _        → opts

setLang ∷ Config → Options → Options
setLang config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "lang" config of
      Just val → if | val == T.pack "tptp" → opts { optLang = TPTP }
                    | val == T.pack "smt"  → opts { optLang = SMT2 }
                    | otherwise            → opts
      _        → opts

setNoInternalEquality ∷ Config → Options → Options
setNoInternalEquality config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "no-internal-equality" config of
      Just val → opts { optNoInternalEquality = val }
      _        → opts

setNoPredicateConstants ∷ Config → Options → Options
setNoPredicateConstants config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "no-predicate-constants" config of
      Just val → opts { optNoPredicateConstants = val }
      _        → opts

setOnlyFiles ∷ Config → Options → Options
setOnlyFiles config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "only-files" config of
      Just val → opts { optOnlyFiles = val }
      _        → opts

setOutputDir ∷ Config → Options → Options
setOutputDir config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "output-dir" config of
      Just val → opts { optOutputDir = val }
      _        → opts

setSchematicFunctions ∷ Config → Options → Options
setSchematicFunctions config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "schematic-functions" config of
      Just val → opts { optSchematicFunctions = val }
      _        → opts

setSchematicPropositionalFunctions ∷ Config → Options → Options
setSchematicPropositionalFunctions config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "schematic-propositional-functions" config of
      Just val → opts { optSchematicPropositionalFunctions = val }
      _        → opts

setSchematicPropositionaSymbol ∷ Config → Options → Options
setSchematicPropositionaSymbol config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "schematic-propositional-symbols" config of
      Just val → opts { optSchematicPropositionalSymbols = val }
      _        → opts

setSnapshotDir ∷ Config → Options → Options
setSnapshotDir config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "snapshot-dir" config of
      Just val → opts { optSnapshotDir = val }
      _        → opts

setSnapshotNoError ∷ Config → Options → Options
setSnapshotNoError config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "snapshot-no-error" config of
      Just val → opts { optSnapshotNoError = val }
      _        → opts

setSnapshotTest ∷ Config → Options → Options
setSnapshotTest config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "snapshot-test" config of
      Just val → opts { optSnapshotTest = val }
      _        → opts

setTime ∷ Config → Options → Options
setTime config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "time" config of
      Just val → opts { optTime = val }
      _        → opts

setUnprovenNoError ∷ Config → Options → Options
setUnprovenNoError config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "unproven-conjecture-no-error" config of
      Just val → opts { optUnprovenNoError = val }
      _        → opts

setVerbose ∷ Config → Options → Options
setVerbose config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "verbose" config of
      Just val  → case verboseOpt val opts of
                        Right o → o
                        Left _  → opts
      _           → opts

setWithCVC4 ∷ Config → Options → Options
setWithCVC4 config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-cvc4" config of
      Just val → opts { optWithCVC4 = val }
      _        → opts

setWithE ∷ Config → Options → Options
setWithE config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-e" config of
      Just val → opts { optWithE = val }
      _        → opts

setWithEquinox ∷ Config → Options → Options
setWithEquinox config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-equinox" config of
      Just val → opts { optWithEquinox = val }
      _        → opts

setWithIleanCoP ∷ Config → Options → Options
setWithIleanCoP config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-ileancop" config of
      Just val → opts { optWithIleanCoP = val }
      _        → opts

setWithMetis ∷ Config → Options → Options
setWithMetis config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-metis" config of
      Just val → opts { optWithMetis = val }
      _        → opts

setWithOnlineATPs ∷ Config → Options → Options
setWithOnlineATPs config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-onlineatps" config of
      Just val → opts { optWithOnlineATPs = val }
      _        → opts

setWithSPASS ∷ Config → Options → Options
setWithSPASS config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-spass" config of
      Just val → opts { optWithSPASS = val }
      _        → opts

setWithtptp4X ∷ Config → Options → Options
setWithtptp4X config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-tptp4X" config of
      Just val → opts { optWithtptp4X = val }
      _        → opts

setWithVampire ∷ Config → Options → Options
setWithVampire config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-vampire" config of
      Just val → opts { optWithVampire = val }
      _        → opts

setWithZ3 ∷ Config → Options → Options
setWithZ3 config opts = newOpts
  where
    newOpts ∷ Options
    newOpts = case lookConfig "with-z3" config of
      Just val → opts { optWithZ3 = val }
      _        → opts

setters ∷ [Config → Options → Options]
setters = [
    setATP
  , setCheck
  , setDumpTypes
  , setFnConstant
  , setIncludePath
  , setLang
  , setNoInternalEquality
  , setNoPredicateConstants
  , setOnlyFiles
  , setOutputDir
  , setSchematicFunctions
  , setSchematicPropositionalFunctions
  , setSchematicPropositionaSymbol
  , setSnapshotDir
  , setSnapshotNoError
  , setSnapshotTest
  , setTime
  , setUnprovenNoError
  , setVerbose
  , setWithCVC4
  , setWithE
  , setWithEquinox
  , setWithIleanCoP
  , setWithMetis
  , setWithOnlineATPs
  , setWithSPASS
  , setWithtptp4X
  , setWithVampire
  , setWithZ3
  ]

combineOptions ∷ Config → Options
combineOptions config = setVals defaultOptions
  where
    setVals ∷ Options → Options
    setVals = foldl (flip (.)) id g

    g ∷ [Options → Options]
    g = map (\f → f config) setters


finalConfig ∷ [Config] → Options
finalConfig []  = defaultOptions
finalConfig cfs = combineOptions $ Config $ HashMap.unions hs
  where
    hs ∷ [Object]
    hs = map getHashMap cfs

    getHashMap ∷ Config → Object
    getHashMap (Config o) = o

loadYAML ∷ FilePath → IO Config
loadYAML dotApia = do
  decoded ← YamlInclude.decodeFile dotApia
  return $ Config $ fromMaybe HashMap.empty decoded

apiaFileName ∷ FilePath
apiaFileName = ".apia"

apiaTemplate :: FilePath
apiaTemplate = "apia.yml"

-- | Get the default values for the command-line 'Options'.
getDefaults ∷ IO Options
getDefaults = do
  paths ← sequence [getCurrentDirectory, getHomeDirectory]
  userFiles ← filterM doesFileExist $ map (</> apiaFileName) paths
  defaultApia ← getDataFileName apiaTemplate

  let allFiles ∷ [FilePath]
      allFiles = userFiles ++ [defaultApia]

  loaded ← mapM loadYAML allFiles
  return $ finalConfig loaded
