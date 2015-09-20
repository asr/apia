------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.Translation
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Select the types required by the TPTP formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.TPTP.TypesSelector ( typesInConjecture ) where

------------------------------------------------------------------------------

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Internal      ( Type )

import Agda.Syntax.Common
  ( TPTPRole(TPTPConjecture, TPTPType)
  )

import Agda.TypeChecking.Monad.Base ( Definition(defType) )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import qualified Agda.Utils.Pretty as AP

import Apia.Logic.Translation.ToTypes ( agdaTypeToType )
import Apia.Logic.Types
  ( LFormula( And
            , Eq
            , Equiv
            , Exists
            , FALSE
            , ForAll
            , Implies
            , Not
            , Or
            , Predicate
            , TRUE
           )
  , LTerm(Var)
  , LType(QuantifierType)
  , TypeName
  )

import Apia.Monad.Base              ( T )
import Apia.Monad.Reports           ( reportDLn )
import Apia.TPTP.Types              ( AF(AFor, AType) )
import Apia.Utils.PrettyPrint       ( (<>), Pretty(pretty) )
import Apia.Utils.AgdaAPI.Interface ( qNameDefinition )

#include "undefined.h"
------------------------------------------------------------------------------

agdaTypesInFormula ∷ LFormula → [(TypeName, QName)]
agdaTypesInFormula TRUE            = []
agdaTypesInFormula FALSE           = []

agdaTypesInFormula (Predicate _ _) = []  -- TODO (18 September 2015).
agdaTypesInFormula (Eq _ _)        = []

agdaTypesInFormula (Not f)         = agdaTypesInFormula f
agdaTypesInFormula (And f1 f2)     = agdaTypesInFormula f1 ++ agdaTypesInFormula f2
agdaTypesInFormula (Or f1 f2)      = agdaTypesInFormula f1 ++ agdaTypesInFormula f2
agdaTypesInFormula (Implies f1 f2) = agdaTypesInFormula f1 ++ agdaTypesInFormula f2
agdaTypesInFormula (Equiv f1 f2)   = agdaTypesInFormula f1 ++ agdaTypesInFormula f2

agdaTypesInFormula (ForAll _ Nothing _) = __IMPOSSIBLE__
agdaTypesInFormula (ForAll vName (Just (QuantifierType tyName qName)) f) =
  (tyName, qName) : agdaTypesInFormula (f $ Var vName)
agdaTypesInFormula ForAll{} = __IMPOSSIBLE__

agdaTypesInFormula (Exists _ Nothing _) = __IMPOSSIBLE__
agdaTypesInFormula (Exists vName (Just (QuantifierType tyName qName)) f) =
  (tyName, qName) : agdaTypesInFormula (f $ Var vName)
agdaTypesInFormula Exists{} = __IMPOSSIBLE__

agdaTypesInConjecture ∷ AF → [(TypeName, QName)]
agdaTypesInConjecture (AFor _ TPTPConjecture f) = agdaTypesInFormula f
agdaTypesInConjecture _                         = __IMPOSSIBLE__

toAType ∷ (TypeName, QName) → T AF
toAType (tyName, qName) = do
  ty ∷ Type ← defType <$> qNameDefinition qName

  -- We run the translation from Agda types to the target logic types.
  lTy ← agdaTypeToType tyName ty

  reportDLn "toAType" 10 $
    pretty "The logical type of " <> AP.pretty qName
    <> pretty " is:\n" <> pretty lTy

  return $ AType qName TPTPType lTy

-- | Returns the types used by a TPTP conjecture.
typesInConjecture ∷ AF → T [AF]
typesInConjecture af = mapM toAType $ agdaTypesInConjecture af
