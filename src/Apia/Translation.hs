
-- | Translation of ATP pragmas to the target logic.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.Translation
  ( conjecturesToAFors
  , generalRolesToAFors
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name ( QName )

import Agda.Syntax.Common
  ( TPTPRole(TPTPAxiom, TPTPConjecture, TPTPDefinition, TPTPHint) )

import Agda.Syntax.Internal       ( Clause, Type )
import Agda.Syntax.Internal.Names ( NamesIn(namesIn) )

import Agda.TypeChecking.Monad.Base
  ( Definition(defName, defType)
  , Definitions
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( ifM )

import qualified Agda.Utils.Pretty as AP

import Apia.Common
  ( Lang(SMT2,TPTP)
  , SMT2Role(SMT2Conjecture, SMT2Declaration)
  )

import Apia.Monad.Base    ( askTOpt, getTDefs, isTVarsEmpty, T)
import Apia.Monad.Reports ( reportDLn, reportSLn )
import Apia.Options       ( Options(optLang) )

import Apia.TargetLang.Types
  ( AF(AExpr, AFor, ATy)
  , ConjectureSet(ConjectureSet)
  , GeneralRoles(GeneralRoles)
  , TargetFormula(FOLFormula, SMT2Expr, SMT2Type)
  )

import Apia.Translation.Functions ( fnToFormula )
import Apia.Translation.Types     ( agdaTypeToFormula )

import Apia.Utils.AgdaAPI.EtaExpansion     ( EtaExpandible(etaExpand) )
-- import Apia.Utils.AgdaAPI.IgnoreSharing    ( IgnoreSharing(ignoreSharing) )

import Apia.Utils.AgdaAPI.Interface
  ( getATPAxioms
  , getATPConjectures
  , getATPHints
  , getClauses
  , getLocalHints
  , isATPDefinition
  , qNameConcreteNameRange
  , qNameDefinition
  , qNameToUniqueString
  )

import Apia.Utils.AgdaAPI.RemoveProofTerms ( removeProofTerm )
import Apia.Utils.AgdaAPI.Vars             ( BoundedVarsType(boundedVarsType) )

import Apia.Utils.PrettyPrint ( (<>), Pretty(pretty) )
import Apia.Utils.Show        ( showListLn, showLn )

import qualified Data.HashMap.Strict as HashMap ( elems, keys )

import qualified Data.Set as Set

#include "undefined.h"

------------------------------------------------------------------------------

toAFor ∷ Maybe TPTPRole → Maybe SMT2Role → QName → Definition → T AF
toAFor tptpRole smt2Role qName def = do
  let ty ∷ Type
      ty = defType def
  reportSLn "toAFor" 10 $
     "Translating QName: " ++ showLn qName
     ++ "The type (pretty-printer):\n" ++ AP.prettyShow ty ++ "\n"
     ++ "The type (show):\n" ++ showLn ty
     ++ "TPTP role: " ++ showLn tptpRole ++ "\n"
     ++ "SMT2 role: " ++ showLn smt2Role ++ "\n"
     ++ "Position: " ++ (showLn . qNameConcreteNameRange) qName

  -- Note (2016-04-01): The Agda current default is not sharing.
  --
  -- We ignore sharing before the translation.
  -- let tyIgnoredSharing ∷ Type
  --     tyIgnoredSharing = ignoreSharing ty

  -- Based on the equality on types, the type and the type ignoring
  -- sharing must be equals.
  -- when (ty /= tyIgnoredSharing) (__IMPOSSIBLE__)

  -- reportSLn "toAFor" 10 $
  --   "The type ignoring sharing is:\n"
  --   ++ "The type (pretty-printer):\n" ++ AP.prettyShow tyIgnoredSharing ++ "\n"
  --   ++ "The type (show):\n" ++ showLn tyIgnoredSharing

  -- We η-expand the type before the translation.
  tyEtaExpanded ← ifM isTVarsEmpty
                      (etaExpand ty)
                      (__IMPOSSIBLE__)

  reportSLn "toAFor" 10 $
    "The η-expanded type is:\n"
    ++ "The type (pretty-printer):\n" ++ AP.prettyShow tyEtaExpanded ++ "\n"
    ++ "The type (show):\n" ++ showLn tyEtaExpanded

  reportSLn "toAFor" 10 $
    if ty == tyEtaExpanded
    then "The type and the η-expanded type: equals"
    else "The type and the η-expanded type: different"

  let boundedVarsTy ∷ [(String, Type)]
      boundedVarsTy = boundedVarsType tyEtaExpanded

  reportSLn "toAFor" 10 $
    "The types of the bounded variables,"
    ++ "i.e. (Abs x _), but not (NoAbs x _) are:\n"
    ++ showListLn boundedVarsTy

  -- We remove the variables which are proof terms from the types.
  tyReady ← foldM removeProofTerm
                  tyEtaExpanded
                  (reverse boundedVarsTy)

  reportSLn "toAFor" 10 $ "tyReady:\n" ++ show tyReady

  -- We run the translation from Agda types to the target logic.
  for ← ifM isTVarsEmpty (agdaTypeToFormula tyReady) (__IMPOSSIBLE__)

  reportDLn "toAFor" 10 $
    pretty "The logic formula for " <> AP.pretty qName
    <> pretty " is:\n" <> pretty for

  case (tptpRole, smt2Role, for) of
    (Just _tptpRole, Nothing, FOLFormula _for) →
      ifM isTVarsEmpty (return $ AFor qName _tptpRole _for) (__IMPOSSIBLE__)
    (Nothing, Just SMT2Conjecture, SMT2Expr _for) →
      ifM isTVarsEmpty (return $ AExpr qName SMT2Conjecture _for) (__IMPOSSIBLE__)
    (Nothing, Just SMT2Declaration, SMT2Type _for) → do
      varName ← qNameToUniqueString qName
      ifM isTVarsEmpty (return $ ATy varName SMT2Declaration _for) (__IMPOSSIBLE__)
    _                  → __IMPOSSIBLE__

-- Translation of an Agda internal function to an annotated formula.
fnToAFor ∷ QName → Definition → T AF
fnToAFor qName def = do
  lang ← askTOpt optLang

  let ty ∷ Type
      ty = defType def

  reportSLn "fnToAFor" 10 $
    "Symbol: " ++ showLn qName
    ++ "Type:\n" ++ showLn ty
    ++ "Position: " ++ (showLn . qNameConcreteNameRange) qName

  -- We get the clauses that define the symbol (all the symbols must
  -- be functions).
  let cls ∷ [Clause]
      cls = getClauses def

  reportSLn "fnToAFor" 10 $
    "Symbol: " ++ showLn qName ++ "Clauses: " ++ show cls

  for ← ifM isTVarsEmpty (fnToFormula qName ty cls) (__IMPOSSIBLE__)
  reportDLn "fnToAFor" 20 $
    pretty "The logic formula for " <> AP.pretty qName
    <> pretty " is:\n" <> pretty for

  case (lang, for) of
    (TPTP, FOLFormula _for) → return $ AFor qName TPTPDefinition _for
    _                       → __IMPOSSIBLE__

-- We translate a local hint to annotated formula.
localHintToAFor ∷ QName → T AF
localHintToAFor qName =
  qNameDefinition qName >>= toAFor (Just TPTPHint) Nothing qName

-- We translate the local hints of an ATP conjecture to annotated
-- formulae.
--
-- Invariant: The 'Definition' must be an ATP conjecture.
localHintsToAFors ∷ Definition → T [AF]
localHintsToAFors def = do
  let hints ∷ [QName]
      hints = getLocalHints def

  reportSLn "localHintsToAFors" 20 $
    "The local hints for the conjecture " ++ (show . defName) def
    ++ " are:\n" ++ show hints

  mapM localHintToAFor hints

-- If a 'QName' is an ATP definition then we required it.
requiredQName ∷ QName → T [AF]
requiredQName qName = do
  qNameDef ← qNameDefinition qName

  -- We don't have recursive ATP definitions, therefore we don't get
  -- duplicates ones from this function.
  if isATPDefinition qNameDef
    then liftM2 (:)
                (fnToAFor qName qNameDef)
                (requiredATPDefsByATPDefinition qNameDef)
    else return []

-- If we required an ATP definition, we also required the ATP
-- definitions used in its definition.
requiredATPDefsByATPDefinition ∷ Definition → T [AF]
requiredATPDefsByATPDefinition def = do
  -- The cls must be unitary, but it was checked elsewhere.
  let cls ∷ [Clause]
      cls = getClauses def

  fmap (nub . concat) (mapM requiredQName $ Set.toList $ namesIn cls)

requiredATPDefsByLocalHints ∷ Definition → T [AF]
requiredATPDefsByLocalHints def = do
  let hints ∷ [QName]
      hints = getLocalHints def

  hintsDefs ← mapM qNameDefinition hints

  fmap (nub . concat) (mapM requiredATPDefsByDefinition hintsDefs)

conjectureToAFor ∷ QName → Definition → T ConjectureSet
conjectureToAFor qName def = do
  lang ← askTOpt optLang

  liftM5
    ConjectureSet
    (case lang of
       TPTP → toAFor (Just TPTPConjecture) Nothing qName def
       SMT2 → toAFor Nothing (Just SMT2Conjecture) qName def
    )
    (requiredATPDefsByDefinition def)
    (localHintsToAFors def)
    (requiredATPDefsByLocalHints def)
    (case lang of
       TPTP → return []
       SMT2 → requiredDeclsByDefinition def
    )

-- | Translate the ATP conjectures and their local hints in the top
-- level module to annotated formulae.
conjecturesToAFors ∷ Definitions → T [ConjectureSet]
conjecturesToAFors topLevelDefs = do
  let conjecturesDefs ∷ Definitions
      conjecturesDefs = getATPConjectures topLevelDefs

  reportSLn "conjecturesToAFors" 20 $
    "Conjectures:\n" ++ (show . HashMap.keys) conjecturesDefs

  zipWithM conjectureToAFor
           (HashMap.keys conjecturesDefs)
           (HashMap.elems conjecturesDefs)

-- We translate the ATP axioms to annonated formulae.
axiomsToAFors ∷ T [AF]
axiomsToAFors = do
  axDefs ∷ Definitions ← getATPAxioms <$> getTDefs

  zipWithM
    (toAFor (Just TPTPAxiom) Nothing)
    (HashMap.keys axDefs)
    (HashMap.elems axDefs)

requiredATPDefsByDefinition ∷ Definition → T [AF]
requiredATPDefsByDefinition def =
  fmap (nub . concat) (mapM requiredQName $ Set.toList $ namesIn def)

requiredATPDefsByAxioms ∷ T [AF]
requiredATPDefsByAxioms = do
  axDefs ∷ Definitions ← getATPAxioms <$> getTDefs

  fmap (nub . concat) (mapM requiredATPDefsByDefinition (HashMap.elems axDefs))

-- We translate the ATP general hints to annonated formulae.
generalHintsToAFors ∷ T [AF]
generalHintsToAFors = do
  ghDefs ∷ Definitions ← getATPHints <$> getTDefs

  zipWithM
    (toAFor (Just TPTPHint) Nothing)
    (HashMap.keys ghDefs)
    (HashMap.elems ghDefs)

requiredATPDefsByHints ∷ T [AF]
requiredATPDefsByHints = do
  ghDefs ∷ Definitions ← getATPHints <$> getTDefs

  fmap (nub . concat) (mapM requiredATPDefsByDefinition (HashMap.elems ghDefs))

-- | Translate the ATP axioms, the ATP general hints, and the ATP
-- required definitions in the top level module and its imported
-- modules to annotated formulae.
generalRolesToAFors ∷ T GeneralRoles
generalRolesToAFors = liftM4 GeneralRoles
                             axiomsToAFors
                             requiredATPDefsByAxioms
                             generalHintsToAFors
                             requiredATPDefsByHints

requiredDeclsByDefinition ∷ Definition → T [AF]
requiredDeclsByDefinition def = do
  -- We get all the @QNames@ in the definition.
  let qNamesInDef ∷ [QName]
      qNamesInDef = Set.toList $ namesIn def

  defsInDefTy ← mapM qNameDefinition qNamesInDef

  zipWithM (toAFor Nothing (Just SMT2Declaration)) qNamesInDef defsInDefTy
