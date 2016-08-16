
-- | TPTP types and common functions on them.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.TPTP.Types
  ( AF(AFor)
  , allRequiredDefs
  , commonRequiredDefs
  , ConjectureSet(defsConjecture
                 , defsLocalHints
                 , localHintsConjecture
                 , ConjectureSet
                 , theConjecture
                 )
  , dropCommonRequiredDefs
  , GeneralRoles(axioms, defsAxioms, defsHints, hints, GeneralRoles)
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common        ( TPTPRole )

import Apia.FOL.Types  ( LFormula )
import Apia.Utils.List ( duplicate, duplicatesElements )

------------------------------------------------------------------------------
-- Note: We don't import the module TPTP.ConcreteSyntax to avoid a circular
-- importation, therefore Haddock does not create a link for
-- 'TPTP.ConcreteSyntax.ToTPTP'.

-- | Annotated formulae.
--
-- The annotated formulae are not in TPTP (FOF) concrete syntax.
data AF = AFor QName TPTPRole LFormula

instance Eq AF where
  (AFor qName1 _ _) == (AFor qName2 _ _) = qName1 == qName2

instance Ord AF where
  compare (AFor qName1 _ _) (AFor qName2 _ _) = compare qName1 qName2

instance Show AF where
  show (AFor qname _ _) = show qname

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
