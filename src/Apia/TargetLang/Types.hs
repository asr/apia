
-- | Target language types and common functions on them.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.TargetLang.Types
  ( AF(AExpr, AFor, ATy)
  , allRequiredDefs
  , commonRequiredDefs
  , ConjectureSet( declsConjecture
                 , defsConjecture
                 , defsLocalHints
                 , localHintsConjecture
                 , ConjectureSet
                 , theConjecture
                 )
  , dropCommonRequiredDefs
  , GeneralRoles(axioms, defsAxioms, defsHints, hints, GeneralRoles)
  , TargetFormula(FOLFormula, SMT2Expr, SMT2Type)
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common        ( TPTPRole )
import Agda.Utils.Impossible     ( Impossible(Impossible), throwImpossible )

import Apia.Common            ( SMT2Role )
import Apia.FOL.Types         ( LFormula )
import Apia.Utils.List        ( duplicate, duplicatesElements )
import Apia.Utils.PrettyPrint ( (<>), Pretty(pretty) )
import Apia.Utils.SMT2        ( SMT2Expr, SMT2Type )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Annotated TPTP (FOF) formulae, SMT-LIB v2 expresions or
-- SMT-LIB v2 types.
data AF = AFor  QName  TPTPRole LFormula
        | AExpr QName  SMT2Role SMT2Expr
        | ATy   String SMT2Role SMT2Type

instance Eq AF where
  (AFor qName1 _ _)  == (AFor qName2 _ _)  = qName1 == qName2
  (AExpr qName1 _ _) == (AExpr qName2 _ _) = qName1 == qName2
  (ATy xs1 _ _)      == (ATy xs2 _ _)      = xs1    == xs2
  _                  == _                  = __IMPOSSIBLE__

instance Ord AF where
  compare (AFor qName1 _ _)  (AFor qName2 _ _)  = compare qName1 qName2
  compare (AExpr qName1 _ _) (AExpr qName2 _ _) = compare qName1 qName2
  compare (ATy xs1 _ _)      (ATy xs2 _ _)      = compare xs1 xs2
  compare _                  _                  = __IMPOSSIBLE__

instance Show AF where
  show (AFor qname _ _)  = show qname
  show (AExpr qname _ _) = show qname
  show (ATy name _ _)    = show name

-- | The 'ATPRole's share by all the conjetures in an Agda module.
data GeneralRoles = GeneralRoles
  { axioms     ∷ [AF]  -- ^ The axioms.
  , defsAxioms ∷ [AF]  -- ^ ATP definitions used by the axioms.
  , hints      ∷ [AF]  -- ^ The general hints.
  , defsHints  ∷ [AF]  -- ^ ATP definitions used by the general hints.
  }

-- | The 'ATPRole's associated with a conjecture.
data ConjectureSet = ConjectureSet
  { theConjecture        ∷ AF    -- ^ The conjecture.
  , defsConjecture       ∷ [AF]  -- ^ ATP definitions used by the conjecture.
  , localHintsConjecture ∷ [AF]  -- ^ The conjecture local hints.
  , defsLocalHints       ∷ [AF]  -- ^ ATP definitions used by the local hints.
  , declsConjecture      ∷ [AF]  -- ^ ATP declarations used by the conjecture.
  }

-- | All required definitions by a conjecture.
allRequiredDefs ∷ GeneralRoles → ConjectureSet → [AF]
allRequiredDefs generalRoles conjectureSet =
  defsAxioms generalRoles
  ++ defsHints generalRoles
  ++ defsLocalHints conjectureSet
  ++ defsConjecture conjectureSet

-- | Common required definitions by a conjecture.
commonRequiredDefs ∷ GeneralRoles → ConjectureSet → [AF]
commonRequiredDefs generalRoles conjectureSet =
  if not $ duplicate allDefs then [] else duplicatesElements $ sort allDefs
  where
  allDefs ∷ [AF]
  allDefs = allRequiredDefs generalRoles conjectureSet

-- | Drop the common required definitions by a conjecture.
dropCommonRequiredDefs ∷ GeneralRoles → ConjectureSet →
                         (GeneralRoles, ConjectureSet)
dropCommonRequiredDefs generalRoles conjectureSet =
  if null commonDefs
  then (generalRoles, conjectureSet)
  else
    ( generalRoles { defsAxioms = w
                   , defsHints  = x
                   }
    , conjectureSet { defsLocalHints = y
                    , defsConjecture = z
                    }
    )
  where
  commonDefs ∷ [AF]
  commonDefs = commonRequiredDefs generalRoles conjectureSet

  w, x, y, z ∷ [AF]
  w          = defsAxioms     generalRoles  \\ commonDefs
  x          = defsHints      generalRoles  \\ commonDefs
  y          = defsLocalHints conjectureSet \\ commonDefs
  z          = defsConjecture conjectureSet \\ commonDefs

data TargetFormula = FOLFormula LFormula
                   | SMT2Expr   SMT2Expr
                   | SMT2Type   SMT2Type

instance Pretty TargetFormula where
  pretty (FOLFormula f) = pretty "FOLFormula: " <> pretty f
  pretty (SMT2Expr f)   = pretty "SMT2Exprf: " <> pretty f
  pretty (SMT2Type ty)  = pretty "SMT2Type: " <> pretty ty
