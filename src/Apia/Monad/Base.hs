
-- | The translation monad.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Monad.Base
  ( askTOpt
  , checkExecutable
  , getTATPs
  , getTDefs
  , getTVars
  , isTVarsEmpty
  , modifyTATPs
  , modifyTDefs
  , newTVar
  , popTVar
  , pushTNewVar
  , pushTVar
  , runT
  , T
  , tCatch
  , tError
  , TError ( IncompatibleCLOptions
           , MissingFile
           , MissingInputFile
           , MissingInterfaceFile
           , MissingTPTP4XCommand
           , MissingTPTP4XCommandZ3
           , NoATP
           , NoATPsProof
           , NoFOLDefinition
           , NoImplementedOption
           , NoOneClauseDefinition
           , NoSupportedATPVersion
           , ProofTermInDefintion
           , RemoveProofTermError
           , SnapshotDifferentFiles
           , SnapshotSameDirectory
           , TPTP4XErrorWarning
           , TranslationOfWildCardPatterns
           , UniversalQuantificationError
           , UnknownATP
           , WrongATPCommand
           , WrongInterfaceFile
           )
  , TState  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name    ( QName )
import Agda.Syntax.Internal         ( Term )
import Agda.TypeChecking.Monad.Base ( Definitions )
import Agda.Utils.Impossible        ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Maybe             ( caseMaybeM )

import qualified Agda.Utils.Pretty as AP

import Apia.Common            ( ATP(Z3) )
import Apia.Monad.Environment ( env )
import Apia.Options           ( Options )

import qualified Apia.Utils.Except as E

import Apia.Utils.Name  ( freshName )

import Apia.Utils.PrettyPrint
  ( (<>)
  , cquotes
  , Doc
  , Pretty(pretty)
  , scquotes
  , sspaces
  )

import Control.Monad.Reader ( ask, ReaderT(runReaderT) )

import Control.Monad.State
  ( evalState
  , evalStateT
  , get
  , modify
  , put
  , StateT
  )

import qualified Data.HashMap.Strict as HashMap ( empty )

import System.Directory ( findExecutable )

#include "undefined.h"

------------------------------------------------------------------------------
-- | The translation monad state.

-- See note [@OptionsPragma@].
data TState = TState
  { tDefs ∷ Definitions  -- ^ Agda definitions.
  , tVars ∷ [String]     -- ^ Variables names.
  , tATPs ∷ [ATP]        -- ^ Selected ATPs.
  }

-- The initial state.
initTState ∷ TState
initTState = TState { tDefs = HashMap.empty
                    , tVars = []
                    , tATPs = []
                    }

------------------------------------------------------------------------------
-- Errors

-- | The errors in the translation monad.
data TError = IncompatibleCLOptions String String
            | MissingFile FilePath
            | MissingInputFile
            | MissingInterfaceFile FilePath
            | MissingTPTP4XCommand String
            | MissingTPTP4XCommandZ3 String
            | NoATP
            | NoATPsProof FilePath
            | NoFOLDefinition QName
            | NoImplementedOption String
            | NoOneClauseDefinition QName
            | NoSupportedATPVersion ATP String
            | ProofTermInDefintion QName
            | RemoveProofTermError Term
            | SnapshotDifferentFiles FilePath FilePath
            | SnapshotSameDirectory
            | TPTP4XErrorWarning FilePath String String
            | TranslationOfWildCardPatterns
            | UniversalQuantificationError String
            | UnknownATP String
            | WrongATPCommand ATP String
            | WrongInterfaceFile FilePath

-- | Throw an error in the the translation monad
tError ∷ TError → T a
tError = E.throwE

-- | Catch errors in the the translation monad
tCatch ∷ T () → (TError → T ()) -> T ()
tCatch = E.catchE

-- TODO (2017-01-03): Find a different module for this function.
-- | @checkExecutable file msg@ throws an exception with message @msg@
-- if the executable @file@ is missing.
checkExecutable ∷ FilePath → TError → T ()
checkExecutable file err =
  caseMaybeM (liftIO $ findExecutable file)
             (tError err)
             (\_ → return ())

instance Pretty TError where
  pretty (IncompatibleCLOptions opt1 opt2) =
    "the " <> scquotes opt1 <> " and " <> scquotes opt2
    <> " options are incompatible"

  pretty (MissingFile file) = "the file " <> pretty file <> " does not exist"

  pretty MissingInputFile = "missing input file (try --help)"

  pretty (MissingInterfaceFile file) =
   "the interface file " <> pretty file
   <> " does not exist (use Agda to generate it)"

  pretty (MissingTPTP4XCommand cmd) =
    "the " <> pretty cmd <> " command from the TPTP library does not exist"

  pretty (MissingTPTP4XCommandZ3 cmd) =
    "the " <> pretty cmd <> " command from the TPTP library does not exist"
    <> " and it is required for using " <> pretty Z3 <> " as a first-order ATP"

  pretty NoATP = "at least you need to specify one ATP"

  pretty (NoATPsProof file) =
    "the ATP(s) did not prove the conjecture in " <> pretty file

  pretty (NoFOLDefinition qName) =
    "the translation of " <> cquotes (AP.pretty qName)
    <> " failed because it is not a FOL-definition"

  pretty (NoImplementedOption opt) =
    "the option " <> scquotes opt <> " is not implemented"

  pretty (NoOneClauseDefinition qName) =
    "the translation of " <> cquotes (AP.pretty qName)
    <> " failed because its definition only can have a clause"

  pretty (NoSupportedATPVersion atp version) =
    "the ATP " <> pretty atp <> " version " <> pretty version
    <> " is not supported"

  pretty (ProofTermInDefintion qName) =
   "the translation of " <> cquotes (AP.pretty qName)
   <> " failed because we do not how to erase proof terms in the definitions"

  pretty (RemoveProofTermError term) =
    "the translation failed because we do not know how erase "
    <> "the term\n" <> (pretty . show) term

  pretty (SnapshotDifferentFiles f1 f2) =
    "the files are different:\n" <> pretty f1 <> "\n" <> pretty f2

  pretty SnapshotSameDirectory =
   "the " <> scquotes "--output-dir" <> " and " <> scquotes "--snapshot-dir"
   <> " options cannot be the same"

  pretty TranslationOfWildCardPatterns =
    "the translation of wild card patterns is not implemented"

  pretty (TPTP4XErrorWarning file tptp4XExec out) =
    pretty tptp4XExec <> sspaces "found an error/warning in the file"
    <> pretty file
    <> "\nPlease report this as a bug\n\n" <> pretty out

  pretty (UniversalQuantificationError opt) =
    "use the " <> scquotes opt
    <> " option for the translation of first-order logic universal quantified "
    <> pretty entities
    where
      entities ∷ Doc
      entities =
        case opt of
          "--schematic-functions"               → "functions"
          "--schematic-propositional-symbols"   → "propositional symbols"
          "--schematic-propositional-functions" → "propositional functions"
          _                                     → __IMPOSSIBLE__

  pretty (UnknownATP name) = "the ATP " <> scquotes name <> " is unknown"

  pretty (WrongATPCommand atp cmd) =
   "the " <> scquotes cmd <> " command associated with "
   <> pretty atp <> " does not exist"

  pretty (WrongInterfaceFile file) =
    "The reading of the interface file " <> pretty file <> " failed. "
    <> "It is possible that you used a different version "
    <> "of Agda to build the Apia program and to type-check your module"

------------------------------------------------------------------------------
-- | The translation monad.
type T = E.ExceptT TError (StateT TState (ReaderT Options IO))

-- | Running the translation monad.
runT ∷ T a → IO (Either TError a)
runT ta = env >>= runReaderT (evalStateT (E.runExceptT ta) initTState)

-- | Return 'True' if the list of variables in the translation monad
-- state is empty.
isTVarsEmpty ∷ T Bool
isTVarsEmpty = fmap (null . tVars) get

-- | Fresh variable.
newTVar ∷ T String
newTVar = fmap (evalState freshName . tVars) get

-- | Pop a variable from the translation monad state.
popTVar ∷ T ()
popTVar = do
  state ← get
  case tVars state of
    []       → __IMPOSSIBLE__
    (_ : xs) → put state { tVars = xs }

-- | Push a variable in the translation monad state.
pushTVar ∷ String → T ()
pushTVar x = do
  state ← get
  put state { tVars = x : tVars state }

-- | Create a fresh variable and push it in the translation monad state.
pushTNewVar ∷ T String
pushTNewVar = newTVar >>= \freshVar → pushTVar freshVar >> return freshVar

-- | Get the ATPs from the translation monad state.
getTATPs ∷ T [ATP]
getTATPs = fmap tATPs get

-- | Get the Agda 'Definitions' from the translation monad state.
getTDefs ∷ T Definitions
getTDefs = fmap tDefs get

-- | Ask for a concrete 'Options' from the translation monad
-- environment.
askTOpt ∷ (Options → a) → T a
askTOpt opt = fmap opt ask

-- | Get the variables from the translation monad state.
getTVars ∷ T [String]
getTVars = fmap tVars get

-- | Modify the ATPs in the translation monad state.
modifyTATPs ∷ [ATP] → T ()
modifyTATPs atps = modify $ \s → s { tATPs = atps }

-- | Modify the Agda 'Definitions' in the translation monad state.
modifyTDefs ∷ Definitions → T ()
modifyTDefs defs = modify $ \s → s { tDefs = defs }
