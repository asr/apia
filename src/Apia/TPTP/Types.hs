------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.Types
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- TPTP types and common functions on them.
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.TPTP.Types
  ( AnF(AnFor, AnType)
  , allRequiredDefs
  , commonRequiredDefs
  , ConjectureSet( defsConjecture
                 , defsLocalHints
                 , localHintsConjecture
                 , ConjectureSet
                 , theConjecture
                 , typesConjecture
                 )
  , dropCommonRequiredDefs
  , GeneralRoles(axioms, defsAxioms, defsHints, hints, GeneralRoles)
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Common        ( TPTPRole )

import Apia.Logic.Types  ( LFormula, LType )
import Apia.Utils.List   ( duplicate, duplicatesElements )

import Data.List ( (\\), sort )

------------------------------------------------------------------------------
-- Note: We don't import the module TPTP.ConcreteSyntax to avoid a circular
-- importation, therefore Haddock does not create a link for
-- 'TPTP.ConcreteSyntax.ToTPTP'.

-- | Annotated formulae.
--
-- The annotated formulae are not in TPTP (FOF or TFF0) concrete
-- syntax.
data AnF = AnFor QName TPTPRole LFormula
         | AnType QName TPTPRole LType

instance Eq AnF where
  (AnFor qName1 _ _)  == (AnFor qName2 _ _)  = qName1 == qName2
  (AnType qName1 _ _) == (AnType qName2 _ _) = qName1 == qName2
  _                   == _                   = False

instance Ord AnF where
  compare (AnFor qName1 _ _)  (AnFor qName2 _ _)  = compare qName1 qName2
  compare AnFor{}             AnType{}            = LT
  compare AnType{}            AnFor{}             = GT
  compare (AnType qName1 _ _) (AnType qName2 _ _) = compare qName1 qName2

instance Show AnF where
  show (AnFor qName _ _)  = show qName
  show (AnType qName _ _) = show qName

-- | The 'ATPRole's share by all the conjetures in an Agda module.
data GeneralRoles = GeneralRoles
  { axioms     ∷ [AnF]  -- ^ The axioms.
  , defsAxioms ∷ [AnF]  -- ^ ATP definitions used by the axioms.
  , hints      ∷ [AnF]  -- ^ The general hints.
  , defsHints  ∷ [AnF]  -- ^ ATP definitions used by the general hints.
  }

-- | The 'ATPRole's associated with a conjecture.
data ConjectureSet = ConjectureSet
  { theConjecture        ∷ AnF    -- ^ The conjecture.
  , typesConjecture      ∷ [AnF]  -- ^ Conjecture's types.
  , defsConjecture       ∷ [AnF]  -- ^ ATP definitions used by the conjecture.
  , localHintsConjecture ∷ [AnF]  -- ^ The conjecture local hints.
  , defsLocalHints       ∷ [AnF]  -- ^ ATP definitions used by the local hints.
  }

-- | All required definitions by a conjecture.
allRequiredDefs ∷ GeneralRoles → ConjectureSet → [AnF]
allRequiredDefs generalRoles conjectureSet =
  defsAxioms generalRoles
  ++ defsHints generalRoles
  ++ defsLocalHints conjectureSet
  ++ defsConjecture conjectureSet

-- | Common required definitions by a conjecture.
commonRequiredDefs ∷ GeneralRoles → ConjectureSet → [AnF]
commonRequiredDefs generalRoles conjectureSet =
  if not $ duplicate allDefs then [] else duplicatesElements $ sort allDefs
  where
  allDefs ∷ [AnF]
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
  commonDefs ∷ [AnF]
  commonDefs = commonRequiredDefs generalRoles conjectureSet

  w, x, y, z ∷ [AnF]
  w          = defsAxioms     generalRoles  \\ commonDefs
  x          = defsHints      generalRoles  \\ commonDefs
  y          = defsLocalHints conjectureSet \\ commonDefs
  z          = defsConjecture conjectureSet \\ commonDefs
