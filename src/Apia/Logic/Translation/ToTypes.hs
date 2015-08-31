------------------------------------------------------------------------------
-- |
-- Module      : Apia.Logic.Translation.ToTypes
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation of Agda internal types to the target logic types.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Logic.Translation.ToTypes
  ( agdaTypeToType
  ) where

import Agda.Syntax.Internal as I
  ( Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Term(Sort)
  , Type
  , Type'(El)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Logic.Types   ( LType(AtomicType), TypeName )
import Apia.Monad.Base    ( T )
import Apia.Monad.Reports ( reportSLn )

#include "undefined.h"

------------------------------------------------------------------------------

agdaTypeToType ∷ TypeName → Type → T LType
agdaTypeToType tyName ty@(El (Type (Max [ClosedLevel 1])) term) = do
  reportSLn "agdaTypeToType" 10 $ "Processing type ty:\n" ++ show ty
  agdaTermToType tyName term

agdaTypeToType _ _ = __IMPOSSIBLE__

agdaTermToType ∷ TypeName → Term → T LType
agdaTermToType tyName (Sort (Type (Max []))) = return $ AtomicType tyName

agdaTermToType _ term = do
  reportSLn "agdaTermToType" 10 $ "Processing term:\n" ++ show term
  __IMPOSSIBLE__
