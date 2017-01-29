
-- | Functions on Agda vars.

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Apia.Utils.AgdaAPI.Vars
  ( BoundedVars(boundedVars)
  , BoundedVarsType(boundedVarsType)
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Common
  ( Arg(Arg)
  , ArgInfo(ArgInfo, argInfoHiding)
  , Dom(Dom)
  , Hiding(NotHidden)
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Elim
  , Elim'(Apply, IApply, Proj)
  , Sort(Type)
  , Term(Con, Def, Lam, Pi, Var)
  , Type
  , Type'(El)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Total of bounded variables in an Agda entity.
class BoundedVars a where
  boundedVars ∷ a → Int

instance BoundedVars Term where
  boundedVars (Def _ elims) = boundedVars elims

  boundedVars (Lam ArgInfo{argInfoHiding = NotHidden} (Abs _ absTerm)) =
    1 + boundedVars absTerm

  boundedVars (Var n _)
    | n >= 0    = 0
    | otherwise = __IMPOSSIBLE__

  boundedVars _ = __IMPOSSIBLE__

instance BoundedVars Elim where
  boundedVars (Apply (Arg _ term)) = boundedVars term
  boundedVars (Proj _ _)           = __IMPOSSIBLE__
  boundedVars IApply{}             = __IMPOSSIBLE__

instance BoundedVars a ⇒ BoundedVars (Arg a) where
  boundedVars (Arg _ e) = boundedVars e

instance BoundedVars a ⇒ BoundedVars [a] where
   boundedVars xs = sum $ map boundedVars xs

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
  boundedVarsType (Pi (Dom _ _ ty) (Abs x absTy)) =
    (x, ty) : boundedVarsType absTy

  boundedVarsType (Def _ elims) = boundedVarsType elims

  boundedVarsType Con{}     = []
  boundedVarsType (Lam _ _) = []

  boundedVarsType (Var n _) | n >= 0    = []
                            | otherwise = __IMPOSSIBLE__

  boundedVarsType _ = __IMPOSSIBLE__

instance BoundedVarsType Elim where
  boundedVarsType (Apply (Arg _ term)) = boundedVarsType term
  boundedVarsType (Proj _ _)           = __IMPOSSIBLE__
  boundedVarsType IApply{}             = __IMPOSSIBLE__

instance BoundedVarsType a ⇒ BoundedVarsType (Arg a) where
  boundedVarsType (Arg _ e) = boundedVarsType e

instance BoundedVarsType a ⇒ BoundedVarsType [a] where
  boundedVarsType = concatMap boundedVarsType
