------------------------------------------------------------------------------
-- |
-- Module      : Apia.Logic.Translation.ToFormulae.Types
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation of Agda internal types to the target logic formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Logic.Translation.ToFormulae.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Common
  ( ArgInfo(ArgInfo, argInfoHiding)
  , Dom(Dom, domInfo, unDom)
  , Hiding(NotHidden)
  )

import Agda.Syntax.Internal as I
  ( Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Type
  , Type'(El)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Logic.Translation.ToFormulae.Terms ( agdaTermToFormula )
import Apia.Logic.Types                        ( LFormula )
import Apia.Monad.Base                         ( T )
import Apia.Monad.Reports                      ( reportSLn )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Translate an Agda internal 'Dom' 'Type' to a target logic
-- formula.
agdaDomTypeToFormula ∷ Dom Type → T LFormula
agdaDomTypeToFormula Dom {domInfo = info, unDom = ty} =
  case info of
    ArgInfo { argInfoHiding = NotHidden } → agdaTypeToFormula ty
    _                                     →  __IMPOSSIBLE__

-- | Translate an Agda internal 'Type' to a target logic formula.
agdaTypeToFormula ∷ Type → T LFormula
agdaTypeToFormula ty@(El (Type (Max [])) term) = do
  reportSLn "agdaTypeToFormula" 10 $ "Processing type ty:\n" ++ show ty
  agdaTermToFormula term

agdaTypeToFormula ty@(El (Type (Max [ClosedLevel 1])) term) = do
  reportSLn "agdaTypeToFormula" 10 $ "Processing type ty:\n" ++ show ty
  agdaTermToFormula term

agdaTypeToFormula _ = __IMPOSSIBLE__
