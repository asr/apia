------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.Translation
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation of ATP pragmas to TPTP formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.TPTP.Translation
  ( conjecturesToAFs
  , generalRolesToAFs
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Applicative ( (<$>) )
import Control.Monad       ( foldM, liftM2, liftM4, zipWithM )

import Data.List    ( nub )

import qualified Data.HashMap.Strict as HashMap ( elems, keys )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Abstract.Name ( QName )

import Agda.Syntax.Common
  ( ATPRole(ATPAxiom, ATPConjecture, ATPDefinition, ATPHint) )

import Agda.Syntax.Internal ( Clause, Type )

import Agda.TypeChecking.Monad.Base
  ( Definition(defName, defType)
  , Definitions
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( ifM )

------------------------------------------------------------------------------
-- Apia imports

import Apia.AgdaInternal.RemoveProofTerms ( removeProofTerm )
import Apia.AgdaInternal.Vars             ( BoundedVarsType(boundedVarsType) )

import Apia.AgdaInternal.EtaExpansion ( EtaExpandible(etaExpand) )

import Apia.AgdaInternal.Interface
  ( getATPAxioms
  , getATPConjectures
  , getATPHints
  , getClauses
  , getLocalHints
  , isATPDefinition
  , qNameDefinition
  , QNamesIn(qNamesIn)
  , qNameConcreteNameRange
  )

import Apia.FOL.Translation.Functions ( fnToFormula )
import Apia.FOL.Translation.Types     ( typeToFormula )
import Apia.Monad.Base                ( getTDefs, isTVarsEmpty, T)
import Apia.Monad.Reports             ( reportSLn )

import Apia.TPTP.Types
  ( AF(AF)
  , ConjectureSet(ConjectureSet)
  , GeneralRoles(GeneralRoles)
  )

import Apia.Utils.Show ( showListLn, showLn )

#include "../undefined.h"

------------------------------------------------------------------------------

toAF ∷ ATPRole → QName → Definition → T AF
toAF role qName def = do
  let ty ∷ Type
      ty = defType def
  reportSLn "toAF" 10 $
     "Translating QName: " ++ showLn qName
     ++ "Type:\n" ++ showLn ty
     ++ "Role: " ++ showLn role ++ "\n"
     ++ "Position: " ++ showLn (qNameConcreteNameRange qName)

  -- We eta-expand the type before the translation.
  tyEtaExpanded ← ifM isTVarsEmpty (etaExpand ty) (__IMPOSSIBLE__)

  reportSLn "toAF" 10 $ "The eta-expanded type is:\n" ++ show tyEtaExpanded

  reportSLn "toAF" 10 $
    if ty == tyEtaExpanded
    then "The type and the eta-expanded type: equals"
    else "The type and the eta-expanded type: different"

  let boundedVarsTy ∷ [(String, Type)]
      boundedVarsTy = boundedVarsType tyEtaExpanded

  reportSLn "toAF" 10 $
    "The types of the bounded variables,"
    ++ "i.e. (Abs x _), but not (NoAbs x _) are:\n"
    ++ showListLn boundedVarsTy

  -- We remove the variables which are proof terms from the types.
  tyReady ← foldM removeProofTerm
                  tyEtaExpanded
                  (reverse boundedVarsTy)

  reportSLn "toAF" 10 $ "tyReady:\n" ++ show tyReady

  -- We run the translation from Agda types to FOL.
  for ← ifM isTVarsEmpty (typeToFormula tyReady) (__IMPOSSIBLE__)

  reportSLn "toAF" 10 $
    "The FOL formula for " ++ show qName ++ " is:\n" ++ show for

  ifM isTVarsEmpty (return $ AF qName role for) (__IMPOSSIBLE__)

-- Translation of an Agda internal function to an AF definition.
fnToAF ∷ QName → Definition → T AF
fnToAF qName def = do
  let ty ∷ Type
      ty = defType def

  reportSLn "symbolToAF" 10 $
    "Symbol: " ++ showLn qName
    ++ "Type:\n" ++ showLn ty
    ++ "Position: " ++ showLn (qNameConcreteNameRange qName)

  -- We get the clauses that define the symbol (all the symbols must
  -- be functions).
  let cls ∷ [Clause]
      cls = getClauses def

  reportSLn "symbolToAF" 10 $
    "Symbol: " ++ showLn qName ++ "Clauses: " ++ show cls

  for ← ifM isTVarsEmpty (fnToFormula qName ty cls) (__IMPOSSIBLE__)
  reportSLn "symbolToAF" 20 $
    "The FOL formula for " ++ show qName ++ " is:\n" ++ show for

  return $ AF qName ATPDefinition for

-- We translate a local hint to an AF.
localHintToAF ∷ QName → T AF
localHintToAF qName = qNameDefinition qName >>= toAF ATPHint qName

-- We translate the local hints of an ATP conjecture to AF's.
-- Invariant: The 'Definition' must be an ATP conjecture.
localHintsToAFs ∷ Definition → T [AF]
localHintsToAFs def = do
  let hints ∷ [QName]
      hints = getLocalHints def

  reportSLn "hintsToFOLs" 20 $
    "The local hints for the conjecture " ++ show (defName def)
    ++ " are:\n" ++ show hints

  mapM localHintToAF hints

-- If a 'QName' is an ATP definition then we required it.
requiredQName ∷ QName → T [AF]
requiredQName qName = do
  qNameDef ← qNameDefinition qName

  -- We don't have recursive ATP definitions, therefore we don't get
  -- duplicates ones from this function.
  if isATPDefinition qNameDef
    then liftM2 (:)
                (fnToAF qName qNameDef)
                (requiredATPDefsByATPDefinition qNameDef)
    else return []

-- If we required an ATP definition, we also required the ATP
-- definitions used in its definition.
requiredATPDefsByATPDefinition ∷ Definition → T [AF]
requiredATPDefsByATPDefinition def = do
  -- We get all the 'QName's in the definition's clauses.
  let cls ∷ [Clause]
      cls = getClauses def

  -- The cls must be unitary, but it was checked elsewhere.
  let qNamesInClause ∷ [QName]
      qNamesInClause = qNamesIn cls

  fmap (nub . concat) (mapM requiredQName qNamesInClause)

requiredATPDefsByLocalHints ∷ Definition → T [AF]
requiredATPDefsByLocalHints def = do
  let hints ∷ [QName]
      hints = getLocalHints def

  hintsDefs ← mapM qNameDefinition hints

  fmap (nub . concat) (mapM requiredATPDefsByDefinition hintsDefs)

conjectureToAF ∷ QName → Definition → T ConjectureSet
conjectureToAF qName def = liftM4 ConjectureSet
                                  (toAF ATPConjecture qName def)
                                  (requiredATPDefsByDefinition def)
                                  (localHintsToAFs def)
                                  (requiredATPDefsByLocalHints def)

-- | Translate the ATP conjectures and their local hints in the top
-- level module to TPTP formulae.
conjecturesToAFs ∷ Definitions → T [ConjectureSet]
conjecturesToAFs topLevelDefs = do
  let conjecturesDefs ∷ Definitions
      conjecturesDefs = getATPConjectures topLevelDefs

  reportSLn "conjecturesToFOLs" 20 $
    "Conjectures:\n" ++ show (HashMap.keys conjecturesDefs)

  zipWithM conjectureToAF
           (HashMap.keys conjecturesDefs)
           (HashMap.elems conjecturesDefs)

-- We translate the ATP axioms to TPTP formulae.
axiomsToAFs ∷ T [AF]
axiomsToAFs = do
  axDefs ∷ Definitions ← getATPAxioms <$> getTDefs

  zipWithM (toAF ATPAxiom) (HashMap.keys axDefs) (HashMap.elems axDefs)

requiredATPDefsByDefinition ∷ Definition → T [AF]
requiredATPDefsByDefinition def = do
  -- We get all the @QNames@ in the definition.
  let qNamesInDef ∷ [QName]
      qNamesInDef = qNamesIn def

  fmap (nub . concat) (mapM requiredQName qNamesInDef)

requiredATPDefsByAxioms ∷ T [AF]
requiredATPDefsByAxioms = do
  axDefs ∷ Definitions ← getATPAxioms <$> getTDefs

  fmap (nub . concat) (mapM requiredATPDefsByDefinition (HashMap.elems axDefs))

-- We translate the ATP general hints to TPTP formulae.
generalHintsToAFs ∷ T [AF]
generalHintsToAFs = do
  ghDefs ∷ Definitions ← getATPHints <$> getTDefs

  zipWithM (toAF ATPHint) (HashMap.keys ghDefs) (HashMap.elems ghDefs)

requiredATPDefsByHints ∷ T [AF]
requiredATPDefsByHints = do
  ghDefs ∷ Definitions ← getATPHints <$> getTDefs

  fmap (nub . concat) (mapM requiredATPDefsByDefinition (HashMap.elems ghDefs))

-- | Translate the ATP axioms, the ATP general hints, and the ATP
-- required definitions in the top level module and its imported
-- modules to TPTP formulae.
generalRolesToAFs ∷ T GeneralRoles
generalRolesToAFs = liftM4 GeneralRoles
                           axiomsToAFs
                           requiredATPDefsByAxioms
                           generalHintsToAFs
                           requiredATPDefsByHints
