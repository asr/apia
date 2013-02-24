------------------------------------------------------------------------------
-- |
-- Module      : AgdaInternal.Vars
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Functions on Agda vars.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

------------------------------------------------------------------------------
module AgdaInternal.Vars
  ( BoundedVars(boundedVars)
  , BoundedVarsType(boundedVarsType)
  )
  where

------------------------------------------------------------------------------
-- Agda libray imports

import Agda.Syntax.Common
  ( Arg(Arg)
  , ArgInfo(ArgInfo, argInfoHiding)
  , Dom(Dom)
  , Hiding(NotHidden)
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Arg
  , ClauseBody(Bind, Body, NoBody)
  , Term(Con, Def, Lam, Pi, Var)
  , Sort(Type)
  , Type(El)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

------------------------------------------------------------------------------
-- Local imports

#include "../undefined.h"

------------------------------------------------------------------------------
-- | Total of bounded variables in an Agda entity.
class BoundedVars a where
  boundedVars ∷ a → Int

instance BoundedVars Term where

  boundedVars (Def _ args) = boundedVars args
  boundedVars (Lam (ArgInfo {argInfoHiding = NotHidden}) (Abs _ absTerm)) =
    1 + boundedVars absTerm
  boundedVars (Var _ _) = 0
  boundedVars _ = __IMPOSSIBLE__

-- Requires TypeSynonymInstances and FlexibleInstances.
instance BoundedVars a ⇒ BoundedVars (I.Arg a) where
  boundedVars (Arg _ e) = boundedVars e

instance BoundedVars a ⇒ BoundedVars [a] where
   boundedVars xs = sum $ map boundedVars xs

instance BoundedVars ClauseBody where
  boundedVars (Bind (Abs _ cBody)) = 1 + boundedVars cBody
  boundedVars (Bind (NoAbs _ _))   = __IMPOSSIBLE__
  boundedVars (Body term)          = boundedVars term
  boundedVars NoBody               = 0

------------------------------------------------------------------------------
-- We only need to remove the variables which are proof terms, so we
-- collect the types of the bounded variables using the type class
-- BoundedVarsType. The de Bruijn indexes are assigned from right to
-- left,
--
-- e.g.  in @(A B C : Set) → ...@, @A@ is 2, @B@ is 1, and @C@ is 0,
--
-- so we need create the list in the same order.

-- | Types of the bounded variables in an Agda entity.
class BoundedVarsType a where
  boundedVarsType ∷ a → [(String, Type)]

instance BoundedVarsType Type where
  boundedVarsType (El (Type _) term) = boundedVarsType term
  boundedVarsType _                  = __IMPOSSIBLE__

instance BoundedVarsType Term where
  boundedVarsType (Pi _ (NoAbs _ absTy)) = boundedVarsType absTy
  boundedVarsType (Pi (Dom _ ty) (Abs x absTy)) = (x, ty) : boundedVarsType absTy

  boundedVarsType (Def _ args) = boundedVarsType args

  boundedVarsType (Con _ _) = []
  boundedVarsType (Lam _ _) = []
  boundedVarsType (Var _ _) = []

  boundedVarsType _ = __IMPOSSIBLE__

-- Requires TypeSynonymInstances and FlexibleInstances.
instance BoundedVarsType a ⇒ BoundedVarsType (I.Arg a) where
  boundedVarsType (Arg _ e) = boundedVarsType e

instance BoundedVarsType a ⇒ BoundedVarsType [a] where
  boundedVarsType = concatMap boundedVarsType
