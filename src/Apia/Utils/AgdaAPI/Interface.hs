------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.AgdaAPI.Interface
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Handling of Agda interface files (*.agdai).
------------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.Utils.AgdaAPI.Interface
  ( getATPAxioms
  , getATPConjectures
  , getATPHints
  , getClauses
  , getImportedInterfaces
  , getLocalHints
  , isATPDefinition
  , isProjection
  , qNameConcreteNameRange
  , qNameDefinition
  , QNamesIn(qNamesIn)
  , qNameLine
  , qNameNameBindingSiteRange
  , qNameType
  , readInterface
  ) where

------------------------------------------------------------------------------
-- Haskell imports

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ( (<$>) )
#endif

import Control.Monad.IO.Class    ( MonadIO(liftIO) )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.State ( evalStateT, get, put, StateT )

import Data.Int ( Int32 )

import qualified Data.HashMap.Strict as HashMap ( filter, lookup )

import Data.Maybe ( fromMaybe )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Interaction.FindFile ( toIFile )
import qualified Agda.Interaction.Imports as A ( getInterface, readInterface )

import Agda.Interaction.Options
  ( CommandLineOptions(optIncludeDirs, optPragmaOptions)
  , defaultOptions
  , defaultPragmaOptions
  , PragmaOptions(optVerbose)
  , Verbosity
  )

import Agda.Syntax.Abstract.Name
  ( ModuleName
  , Name(nameBindingSite, nameConcrete)
  , QName(qnameName)
  )

import Agda.Syntax.Common
  ( Arg(Arg)
  , Dom(Dom)
  , ATPRole(ATPAxiom, ATPConjecture, ATPDefinition, ATPHint) )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Arg
  , Clause(Clause)
  , ClauseBody
  , ClauseBodyF(Bind, Body, NoBody)
  , ConHead(ConHead)
  , Dom
  , Elim
  , Elim'(Apply, Proj)
  , Term(Con, Def, Lam, Pi, Sort, Var)
  , Type
  , Type'(El)
  )

import Agda.Syntax.Position
  ( HasRange(getRange)
  , Interval'(Interval)
  , Position'(posLine)
  , Range
  , rangeToInterval
  )

import Agda.TypeChecking.Monad.Base
  ( Defn(Axiom
        , axATPHints
        , axATPRole
        , conATPRole
        , Constructor
        , Function
        , funATPRole
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

------------------------------------------------------------------------------
-- Apia imports

import Apia.Monad.Base    ( askTOpt, getTDefs, T )
import Apia.Monad.Reports ( reportSLn )
import Apia.Options       ( Options(optIncludePath) )

import Apia.Utils.AgdaAPI.IgnoreSharing ( ignoreSharing )

import qualified Apia.Utils.Except as E

#include "undefined.h"

------------------------------------------------------------------------------

getATPRole ∷ ATPRole → Definitions → Definitions
getATPRole ATPAxiom      = HashMap.filter isATPAxiom
getATPRole ATPConjecture = HashMap.filter isATPConjecture
-- 31 May 2012. We don't have an example of this case.
--
-- getATPRole ATPDefinition = HashMap.filter isATPDefinition
getATPRole ATPDefinition = __IMPOSSIBLE__
getATPRole ATPHint       = HashMap.filter isATPHint

-- | Return the ATP axioms from a set of Agda 'Definitions'.
getATPAxioms ∷ Definitions → Definitions
getATPAxioms = getATPRole ATPAxiom

-- | Return the ATP conjectures from a set of Agda 'Definitions'.
getATPConjectures ∷ Definitions → Definitions
getATPConjectures = getATPRole ATPConjecture

-- getATPDefinitions ∷ Definitions → Definitions
-- getATPDefinitions = getATPRole ATPDefinition

-- | Return the ATP hints from a set of Agda 'Definitions'.
getATPHints ∷ Definitions → Definitions
getATPHints = getATPRole ATPHint

-- Invariant: The @Definition@ must correspond to an ATP conjecture.
-- | Return the ATP local hints associated with an ATP conjecture.
getLocalHints ∷ Definition → [QName]
getLocalHints def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Axiom{} → axATPHints defn
       _       → __IMPOSSIBLE__

-- We do not want any verbosity from the Agda API.
noVerbosity ∷ PragmaOptions
noVerbosity = let agdaOptVerbose ∷ Verbosity
                  agdaOptVerbose = Trie.singleton [] 0
              in defaultPragmaOptions { optVerbose = agdaOptVerbose }


-- See note [Default @optIncludePath@].
agdaCommandLineOptions ∷ T CommandLineOptions
agdaCommandLineOptions = do
  agdaIncludePaths ← askTOpt optIncludePath

  return $ defaultOptions { optIncludeDirs   = Left agdaIncludePaths
                          , optPragmaOptions = noVerbosity
                          }

-- | Read an Agda interface file.
readInterface ∷ FilePath → T Interface
readInterface file = do
  optsCommandLine ← agdaCommandLineOptions

  -- The physical Agda file (used only to test if the file exists).
  pFile ∷ FilePath ← liftIO $ fmap filePath (absolute file)

  unlessM (liftIO $ doesFileExistCaseSensitive pFile)
          (E.throwE $ "the file " ++ pFile ++ " does not exist")

  -- The physical Agda interface file.
  iFile ∷ FilePath ← liftIO $ fmap (filePath . toIFile) (absolute file)

  unlessM (liftIO $ doesFileExistCaseSensitive iFile)
          (E.throwE $ "the interface file " ++ iFile
                      ++ " does not exist (use Agda to generate it)")

  r ∷ Either TCErr (Maybe Interface) ← liftIO $ runTCMTop $
    do setCommandLineOptions optsCommandLine
       A.readInterface iFile

  case r of
    Right (Just i) → return i
    -- This message is not included in the errors test.
    Right Nothing  → E.throwE $
                       "The reading of the interface file "
                       ++ iFile ++ " failed. "
                       ++ "It is possible that you used a different version "
                       ++ "of Agda to build the Apia program and to "
                       ++ "type-check your module"
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
       Axiom{} → case axATPRole defn of
                   Just ATPAxiom      → True
                   Just ATPConjecture → False
                   Just _             → __IMPOSSIBLE__
                   Nothing            → False

       Constructor{} → case conATPRole defn of
                         Just ATPAxiom → True
                         Just _        → __IMPOSSIBLE__
                         Nothing       → False

       _       → False

-- | Return 'True' if an Agda 'Definition' is an ATP conjecture.
isATPConjecture ∷ Definition → Bool
isATPConjecture def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Axiom{} → case axATPRole defn of
                   Just ATPAxiom      → False
                   Just ATPConjecture → True
                   Just _             → __IMPOSSIBLE__
                   Nothing            → False

       _       → False

-- | Return 'True' if an Agda 'Definition' is an ATP definition.
isATPDefinition ∷ Definition → Bool
isATPDefinition def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Function{} → case funATPRole defn of
                      Just ATPDefinition → True
                      -- 31 May 2012. We don't have an example of this
                      -- case.
                      --
                      -- Just ATPHint       → False
                      Just _       → __IMPOSSIBLE__
                      Nothing      → False

       _          → False

-- | Return 'True' if an Agda 'Definition' is an ATP hint.
isATPHint ∷ Definition → Bool
isATPHint def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Function{} → case funATPRole defn of
                      Just ATPDefinition → False
                      Just ATPHint       → True
                      Just _             → __IMPOSSIBLE__
                      Nothing            → False

       _          → False

-- | Return the Agda 'Definition' associated with a 'QName'.
qNameDefinition ∷ QName → T Definition
qNameDefinition qName = do
  allDefs ← getTDefs
  return $ fromMaybe (__IMPOSSIBLE__) $ HashMap.lookup qName allDefs

-- | Return the 'Type' of a 'QName' ignoring sharing.
qNameType ∷ QName → T Type
qNameType qName = ignoreSharing . defType <$> qNameDefinition qName

-- | Return the line where a 'QName' is defined.
qNameLine ∷ QName → Int32
qNameLine qName =
  -- See issue 13.
  case rangeToInterval $ qNameConcreteNameRange qName of
    Nothing              → -1 -- __IMPOSSIBLE__
    Just (Interval s _)  → posLine s

-- | Return the 'Range' of the concrete name of a 'QName'.
qNameConcreteNameRange ∷ QName → Range
qNameConcreteNameRange = getRange . nameConcrete . qnameName

-- | Return the 'Range' of the 'nameBindingSite' of a 'QName'.
qNameNameBindingSiteRange ∷ QName → Range
qNameNameBindingSiteRange = getRange . nameBindingSite . qnameName

-- | Return the 'Clause's associted with an Agda 'Definition'.
getClauses ∷ Definition → [Clause]
getClauses def =
  let defn ∷ Defn
      defn = theDef def
  in case defn of
       Function{} → funClauses defn
       _          → __IMPOSSIBLE__

-- | Return the 'QName's in an entity.
class QNamesIn a where
  qNamesIn ∷ a → [QName]

instance QNamesIn Term where
  qNamesIn term = case ignoreSharing term of
    Con (ConHead qName _ _) args → qName : qNamesIn args

    Def qName args → qName : qNamesIn args

    Lam _ absTerm → qNamesIn absTerm

    Pi domTy absTy → qNamesIn domTy ++ qNamesIn absTy

    Sort _  → []

    Var n args | n >= 0    → qNamesIn args
               | otherwise → __IMPOSSIBLE__

    _ → __IMPOSSIBLE__

instance QNamesIn Type where
  qNamesIn (El _ term) = qNamesIn term

instance QNamesIn Elim where
  qNamesIn (Apply (Arg _ term)) = qNamesIn term
  qNamesIn (Proj _)             = __IMPOSSIBLE__

instance QNamesIn ClauseBody where
  qNamesIn (Body term)          = qNamesIn term
  qNamesIn (Bind absClauseBody) = qNamesIn absClauseBody
  -- 31 May 2012. We don't have an example of this case.
  --
  -- qNamesIn NoBody               = []
  qNamesIn NoBody = __IMPOSSIBLE__

instance QNamesIn Clause where
  qNamesIn (Clause _ _ _ _ body _ _) = qNamesIn body

instance QNamesIn Definition where
  qNamesIn def = qNamesIn $ defType def

instance QNamesIn a ⇒ QNamesIn [a] where
  qNamesIn = concatMap qNamesIn

-- Requires TypeSynonymInstances and FlexibleInstances.
instance QNamesIn a ⇒ QNamesIn (I.Arg a) where
  qNamesIn (Arg _ e) = qNamesIn e

-- Requires TypeSynonymInstances and FlexibleInstances.
instance QNamesIn a ⇒ QNamesIn (I.Dom a) where
  qNamesIn (Dom _ e) = qNamesIn e

instance QNamesIn a ⇒ QNamesIn (Abs a) where
  qNamesIn (Abs _ e)   = qNamesIn e
  qNamesIn (NoAbs _ e) = qNamesIn e

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
-- Note [Default @optIncludePath@].

-- An empty list of relative include directories @(Left [])@ is
-- interpreted as @["."]@ (from
-- Agda.TypeChecking.Monad.Options). Therefore the default of
-- Options.optAgdaIncludePath is @[]@.
