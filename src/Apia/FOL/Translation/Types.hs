------------------------------------------------------------------------------
-- |
-- Module      : Apia.FOL.Translation.Internal.Types
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation of Agda internal types to first-order logic formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.FOL.Translation.Types
  ( domTypeToFormula
  , typeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Common
  ( ArgInfo(ArgInfo, argInfoHiding)
  , Dom(Dom, domInfo, unDom)
  , Hiding(NotHidden)
  )

import Agda.Syntax.Internal as I
  ( Dom
  , Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Type
  , Type'(El)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.FOL.Translation.Terms ( termToFormula )
import Apia.FOL.Types             ( FOLFormula )
import Apia.Monad.Base            ( T )
import Apia.Monad.Reports         ( reportSLn )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Translate an Agda internal 'Dom' 'Type' to a first-order logic
-- formula 'FOLFormula'.
domTypeToFormula ∷ I.Dom Type → T FOLFormula
domTypeToFormula Dom {domInfo = info, unDom = ty} =
  case info of
    ArgInfo { argInfoHiding = NotHidden } → typeToFormula ty
    _                                     →  __IMPOSSIBLE__

-- | Translate an Agda internal 'Type' to a first-order logic formula
-- 'FOLFormula'.
typeToFormula ∷ Type → T FOLFormula
typeToFormula ty@(El (Type (Max [])) term) = do
  reportSLn "typeToFormula" 10 $ "Processing type ty:\n" ++ show ty
  termToFormula term

typeToFormula ty@(El (Type (Max [ClosedLevel 1])) term) = do
  reportSLn "typeToFormula" 10 $ "Processing type ty:\n" ++ show ty
  termToFormula term

typeToFormula _ = __IMPOSSIBLE__
