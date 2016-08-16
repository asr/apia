
-- | Handling of Agda interface files (*.agdai).

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.Utils.AgdaAPI.Interface
  ( getATPAxioms
  , getATPConjectures
  , getATPHints
  , getATPTypes
  , getClauses
  , getImportedInterfaces
  , getLocalHints
  , isATPDefinition
  , isProjection
  , qNameConcreteNameRange
  , qNameDefinition
  , qNameLine
  , qNameNameBindingSiteRange
  , qNameToString
  , qNameToUniqueString
  , qNameType
  , readInterface
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Interaction.FindFile ( toIFile )
import qualified Agda.Interaction.Imports as A ( getInterface, readInterface )

import Agda.Interaction.Options
  ( CommandLineOptions(optIncludePaths, optPragmaOptions)
  , defaultOptions
  , defaultPragmaOptions
  , PragmaOptions(optVerbose)
  , Verbosity
  )

import Agda.Syntax.Abstract.Name
  ( ModuleName
  , Name(nameBindingSite, nameConcrete, nameId)
  , QName(QName, qnameName)
  )

import Agda.Syntax.Common
  ( NameId(NameId)
  , TPTPRole(TPTPAxiom, TPTPConjecture, TPTPDefinition, TPTPHint, TPTPType)
  )

import Agda.Syntax.Internal as I
  ( Clause
  , Type
  )

import Agda.Syntax.Position
  ( HasRange(getRange)
  , Interval'(Interval)
  , Position'(posLine)
  , Range
  , rangeToInterval
  )

import Agda.TypeChecking.Monad.Base
  ( Defn( Axiom
        , axTPTPHints
        , axTPTPRole
        , conTPTPRole
        , Constructor
        , Function
        , funTPTPRole
        , funClauses
        , funProjection
        )
  , Definition(defType, theDef)
  , Definitions
  , Interface(iImportedModules, iModuleName)
  , Projection
  , runTCMTop
  , TCErr
  )

import Agda.TypeChecking.Monad.Options ( setCommandLineOptions )

import Agda.Utils.FileName
  ( absolute
  , doesFileExistCaseSensitive
  , filePath
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( unlessM )

import qualified Agda.Utils.Trie as Trie ( singleton )

import Apia.Monad.Base    ( askTOpt, getTDefs, T )
import Apia.Monad.Reports ( reportSLn )
import Apia.Options       ( Options(optIncludePath) )

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( ignoreSharing )

import qualified Apia.Utils.Except as E

import Apia.Utils.PrettyPrint ( (<>), Pretty(pretty) )

import Control.Monad.IO.Class    ( MonadIO(liftIO) )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State ( evalStateT, get, put, StateT )

import qualified Data.HashMap.Strict as HashMap ( filter, lookup )

#include "undefined.h"

------------------------------------------------------------------------------

getATPRole ∷ TPTPRole → Definitions → Definitions
getATPRole TPTPAxiom      = HashMap.filter isATPAxiom
getATPRole TPTPConjecture = HashMap.filter isATPConjecture
-- 31 May 2012. We don't have an example of this case.
--
-- getTPTPRole TPTPDefinition = HashMap.filter isATPDefinition
getATPRole TPTPDefinition = __IMPOSSIBLE__
getATPRole TPTPHint       = HashMap.filter isATPHint
getATPRole _              = __IMPOSSIBLE__

-- | Return the ATP axioms from a set of Agda 'Definitions'.
getATPAxioms ∷ Definitions → Definitions
getATPAxioms = getATPRole TPTPAxiom

-- | Return the ATP conjectures from a set of Agda 'Definitions'.
getATPConjectures ∷ Definitions → Definitions
getATPConjectures = getATPRole TPTPConjecture

-- getATPDefinitions ∷ Definitions → Definitions
-- getATPDefinitions = getATPRole ATPDefinition

-- | Return the ATP hints from a set of Agda 'Definitions'.
getATPHints ∷ Definitions → Definitions
getATPHints = getATPRole TPTPHint

-- Invariant: The @Definition@ must correspond to an ATP conjecture.
-- | Return the ATP local hints associated with an ATP conjecture.
getLocalHints ∷ Definition → [QName]
getLocalHints def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Axiom{} → axTPTPHints defn
       _       → __IMPOSSIBLE__

-- | Return the ATP types from a set of Agda 'Definitions'.
getATPTypes ∷ Definitions → Definitions
getATPTypes = getATPRole TPTPType

-- We do not want any verbosity from the Agda API.
noVerbosity ∷ PragmaOptions
noVerbosity = let agdaOptVerbose ∷ Verbosity
                  agdaOptVerbose = Trie.singleton [] 0
              in defaultPragmaOptions { optVerbose = agdaOptVerbose }

agdaCommandLineOptions ∷ T CommandLineOptions
agdaCommandLineOptions = do
  agdaIncludePaths ← askTOpt optIncludePath

  return $ defaultOptions { optIncludePaths   = agdaIncludePaths
                          , optPragmaOptions = noVerbosity
                          }

-- | Read an Agda interface file.
readInterface ∷ FilePath → T Interface
readInterface file = do
  optsCommandLine ← agdaCommandLineOptions

  -- The physical Agda file (used only to test if the file exists).
  pFile ∷ FilePath ← liftIO $ fmap filePath (absolute file)

  unlessM (liftIO $ doesFileExistCaseSensitive pFile)
          (E.throwE $ pretty "the file " <> pretty pFile
                      <> pretty " does not exist")

  -- The physical Agda interface file.
  iFile ∷ FilePath ← liftIO $ fmap (filePath . toIFile) (absolute file)

  unlessM (liftIO $ doesFileExistCaseSensitive iFile)
          (E.throwE $ pretty "the interface file " <> pretty iFile
                      <> pretty " does not exist (use Agda to generate it)")

  r ∷ Either TCErr (Maybe Interface) ← liftIO $ runTCMTop $
    do setCommandLineOptions optsCommandLine
       A.readInterface iFile

  case r of
    Right (Just i) → return i
    -- This message is not included in the errors test.
    Right Nothing  → E.throwE $
                       pretty "The reading of the interface file "
                       <> pretty iFile <> pretty " failed. "
                       <> pretty "It is possible that you used a different version "
                       <> pretty "of Agda to build the Apia program and to "
                       <> pretty "type-check your module"
    Left _         → __IMPOSSIBLE__

getInterface ∷ ModuleName → T Interface
getInterface x = do
  optsCommandLine ← agdaCommandLineOptions

  r ← liftIO $ runTCMTop $ do
    setCommandLineOptions optsCommandLine
    A.getInterface x

  case r of
    Right i → return i
    Left _  → __IMPOSSIBLE__

-- | Return 'True' if an Agda 'Definition' is an ATP axiom.
isATPAxiom ∷ Definition → Bool
isATPAxiom def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Axiom{} → case axTPTPRole defn of
                   Just TPTPAxiom      → True
                   Just TPTPConjecture → False
                   Just _              → __IMPOSSIBLE__
                   Nothing             → False

       Constructor{} → case conTPTPRole defn of
                         Just TPTPAxiom → True
                         Just _         → __IMPOSSIBLE__
                         Nothing        → False

       _       → False

-- | Return 'True' if an Agda 'Definition' is an ATP conjecture.
isATPConjecture ∷ Definition → Bool
isATPConjecture def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Axiom{} → case axTPTPRole defn of
                   Just TPTPAxiom      → False
                   Just TPTPConjecture → True
                   Just _              → __IMPOSSIBLE__
                   Nothing             → False

       _       → False

-- | Return 'True' if an Agda 'Definition' is an ATP definition.
isATPDefinition ∷ Definition → Bool
isATPDefinition def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Function{} → case funTPTPRole defn of
                      Just TPTPDefinition → True
                      -- 31 May 2012. We don't have an example of this
                      -- case.
                      --
                      -- Just TPTPHint    → False
                      Just _              → __IMPOSSIBLE__
                      Nothing             → False

       _          → False

-- | Return 'True' if an Agda 'Definition' is an ATP hint.
isATPHint ∷ Definition → Bool
isATPHint def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Function{} → case funTPTPRole defn of
                      Just TPTPDefinition → False
                      Just TPTPHint       → True
                      Just _              → __IMPOSSIBLE__
                      Nothing             → False

       _          → False

-- | Return the Agda 'Definition' associated with a 'QName'.
qNameDefinition ∷ QName → T Definition
qNameDefinition qName = do
  allDefs ← getTDefs
  return $ fromMaybe (__IMPOSSIBLE__) $ HashMap.lookup qName allDefs

-- | Return the 'Type' of a 'QName' ignoring sharing.
qNameType ∷ QName → T Type
qNameType qName = {- ignoreSharing . -} defType <$> qNameDefinition qName

-- | Return the line where a 'QName' is defined.
qNameLine ∷ QName → Int32
qNameLine qName =
  case rangeToInterval $ qNameNameBindingSiteRange qName of
    -- See Issues 13 and 18.
    Nothing              → __IMPOSSIBLE__
    Just (Interval s _)  → posLine s

-- | Return the 'Range' of the concrete name of a 'QName'.
qNameConcreteNameRange ∷ QName → Range
qNameConcreteNameRange = getRange . nameConcrete . qnameName

-- | Return the 'Range' of the 'nameBindingSite' of a 'QName'.
qNameNameBindingSiteRange ∷ QName → Range
qNameNameBindingSiteRange = getRange . nameBindingSite . qnameName

-- | Return an unique 'String' for a 'QName'.

qNameToUniqueString ∷ QName → T String
-- See note [Unique name]
qNameToUniqueString (QName _ name) = do
  let qNameId ∷ NameId
      qNameId = nameId name

  reportSLn "qNameToUniqueString" 20 $ "qNameId : " ++ show qNameId

  case qNameId of
    NameId x i → return $ (show . nameConcrete) name ++ "_"
                          ++ show x ++ "_"
                          ++ show i

-- | Return a 'String' for a 'QName'.
qNameToString ∷ QName → T String
qNameToString (QName _ name) = return $ show $ nameConcrete name

-- | Return the 'Clause's associted with an Agda 'Definition'.
getClauses ∷ Definition → [Clause]
getClauses def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Function{} → funClauses defn
       _          → __IMPOSSIBLE__

-- Adapted from Agda.TypeChecking.Monad.Signature.isProjection.
-- | Is it the 'Qname' a projection?
isProjection ∷ QName → T (Maybe Projection)
isProjection qname = do
  defn ← theDef <$> qNameDefinition qname
  case defn of
    Function { funProjection = result } → return result
    _                                   → return Nothing

------------------------------------------------------------------------------
-- Imported interfaces

importedInterfaces ∷ ModuleName → StateT [ModuleName] T [Interface]
importedInterfaces x = do
  visitedModules ← get

  if x `notElem` visitedModules
    then do
      put $ x : visitedModules

      i ← lift $ getInterface x

      let iModules ∷ [ModuleName]
          iModules = (fst . unzip . iImportedModules) i

      is ← concat <$> mapM importedInterfaces iModules
      return $ i : is
    else return []

-- | Return the Agda interface files recursively imported by the top
-- level interface file.
getImportedInterfaces ∷ Interface → T [Interface]
getImportedInterfaces i = do
  iInterfaces ← concat <$>
    evalStateT (mapM importedInterfaces $ (fst . unzip . iImportedModules) i) []
  reportSLn "ii" 20 $
    "Imported module names: " ++ show (map iModuleName iInterfaces)
  return iInterfaces

------------------------------------------------------------------------------
-- Note [Unique name]

-- Because Agda predicates can be global or local, we use an unique
-- name for translating them. We append to the @qName@ the @qName@'s
-- id (which generates long TPTP names).
