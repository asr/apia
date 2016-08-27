
-- | Translation of Agda internal types to the target logic.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Translation.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

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

import Apia.Monad.Base        ( T )
import Apia.Monad.Reports     ( reportSLn )
import Apia.Translation.Terms ( agdaTermToFormula )
import Apia.TargetLang.Types  ( TargetFormula )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Translate an Agda internal 'Dom' 'Type' to a target logic
-- formula.
agdaDomTypeToFormula ∷ Dom Type → T TargetFormula
agdaDomTypeToFormula Dom {domInfo = info, unDom = ty} =
  case info of
    ArgInfo { argInfoHiding = NotHidden } → agdaTypeToFormula ty
    _                                     →  __IMPOSSIBLE__

-- | Translate an Agda internal 'Type' to a target logic formula.
agdaTypeToFormula ∷ Type → T TargetFormula
agdaTypeToFormula ty@(El (Type (Max [])) term) = do
  reportSLn "agdaTypeToFormula" 10 $ "Processing type ty:\n" ++ show ty
  agdaTermToFormula term

agdaTypeToFormula ty@(El (Type (Max [ClosedLevel 1])) term) = do
  reportSLn "agdaTypeToFormula" 10 $ "Processing type ty:\n" ++ show ty
  agdaTermToFormula term

agdaTypeToFormula _ = __IMPOSSIBLE__
