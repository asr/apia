
-- | Eta expansion of Agda internal types.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.Utils.AgdaAPI.EtaExpansion ( EtaExpandible(etaExpand) ) where

------------------------------------------------------------------------------

import Apia.Prelude hiding ( sort )

import Agda.Syntax.Common
  ( Arg(Arg)
  , Dom(Dom)
  , defaultArgInfo
  , Nat
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , arity
  , Elim
  , Elim'(Apply, Proj)
  , Elims
  , Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Term(Con, Def, Lam, Pi, Sort, Var)
  , Type
  , Type'(El)
  , var
  )

import Agda.TypeChecking.Substitute ( Apply(apply) )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Maybe      ( whenJustM )

import Apia.Monad.Base                  ( newTVar, T )
import Apia.Monad.Reports               ( reportSLn )
import Apia.Utils.AgdaAPI.DeBruijn      ( IncIndex(incIndex) )
-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )
import Apia.Utils.AgdaAPI.Interface     ( isProjection, qNameType )

#include "undefined.h"

------------------------------------------------------------------------------

-- N.B. The class doesn't use the state of the translation monad,
-- therefore it is not necessary to test for a clean state after its
-- use.

-- | Eta-expandible entities.
class EtaExpandible a where
  etaExpand ∷ a → T a

instance EtaExpandible Sort where
  etaExpand sort@(Type (Max _)) = return sort
  etaExpand _                   = __IMPOSSIBLE__

instance EtaExpandible Type where
  etaExpand (El (Type (Max [])) term) = El (Type (Max [])) <$> etaExpand term

  etaExpand (El (Type (Max [ClosedLevel 1])) term) =
    El (Type (Max [ClosedLevel 1])) <$> etaExpand term

  etaExpand _ = __IMPOSSIBLE__

instance EtaExpandible Term where
  etaExpand (Def qName elims) = do
    whenJustM (isProjection qName) (__IMPOSSIBLE__)

    defTy ∷ Type ← qNameType qName

    let qNameArity ∷ Nat
        qNameArity = arity defTy

    -- Although a term has the right number of arguments, it can be
    -- η-contracted. For example, given ∃ : (A : D → Set) → Set, the
    -- term ∃ A is η-contracted, so we should η-expand it to
    -- ∃ (λ x → A x).
    if qNameArity == length elims
      then case defTy of
        El (Type (Max [ClosedLevel 1]))
           (Pi (Dom _ (El (Type (Max [ClosedLevel 1]))
                          (Pi (Dom _ (El (Type (Max [])) _))
                              (NoAbs _ (El (Type (Max [ClosedLevel 1]))
                                       (Sort (Type (Max [])))))))) _)  →
           case elims of
             [Apply (Arg _ term')] →
               case term' of
                 (Def _ _) → Def qName <$> etaExpand elims

                 (Lam _ _) → Def qName <$> etaExpand elims

                 (Var 0 []) → do
                   freshVar ← newTVar

                   let newArg ∷ Arg Term
                       newArg = Arg defaultArgInfo
                                  (Lam defaultArgInfo
                                  (Abs freshVar (Var 1 [Apply (Arg defaultArgInfo (var 0))])))

                   return $ Def qName [Apply newArg]

                 someTerm → do
                   reportSLn "etaExpansion" 20 $ "someTerm: " ++ show someTerm
                   __IMPOSSIBLE__

             someElims → do
               reportSLn "etaExpansion" 20 $ "someElims: " ++ show someElims
               __IMPOSSIBLE__

        _ → Def qName <$> etaExpand elims

      else do
        -- The η-contraction can *only* reduces by 1 the number of
        -- arguments of a term, for example: Given P : D → Set,
        -- λ x → P x η-contracts to P or given _≡_ : D → D → Set,
        -- (x : D) → (λ y → x ≡ y) η-contracts to (x : D) → _≡_ x.

        -- We applied the η-expansion in this case.
        when (qNameArity - 1 /= length elims) (__IMPOSSIBLE__)

        -- Because we are going to add a new abstraction, we need
        -- increase by one the numbers associated with the variables
        -- in the arguments.
        incVarsEtaExpanded ∷ Elims ← map incIndex <$> etaExpand elims

        let newVar ∷ Arg Term
            newVar = Arg defaultArgInfo (var 0)

        freshVar ← newTVar

        return $ Lam defaultArgInfo
                     (Abs freshVar (Def qName incVarsEtaExpanded `apply` [newVar]))

  -- We don't know an example of η-contraction with Con, therefore
  -- we don't do anything.
  etaExpand term'@(Con _ _) = return term'

  etaExpand (Lam h (Abs x termAbs)) = Lam h . Abs x <$> etaExpand termAbs

  -- It seems it is not necessary to η-expand the domTy in the case
  -- of Pi _ (Abs _ _).
  etaExpand (Pi domTy (Abs x absTy)) = Pi domTy . Abs x <$> etaExpand absTy

  etaExpand (Pi domTy (NoAbs x absTy)) = do
    tDom ← etaExpand domTy
    tAbs ← etaExpand absTy
    return $ Pi tDom (NoAbs x tAbs)

  etaExpand (Sort sort) = Sort <$> etaExpand sort

  etaExpand (Var n args)
    | n >= 0    = Var n <$> etaExpand args
    | otherwise = __IMPOSSIBLE__

  etaExpand term' = do
    reportSLn "etaExpansion" 20 $ "term': " ++ show term'
    __IMPOSSIBLE__

instance EtaExpandible Elim where
  etaExpand (Apply (Arg color term)) = Apply . Arg color <$> etaExpand term
  etaExpand (Proj _)                 = __IMPOSSIBLE__

instance EtaExpandible a ⇒ EtaExpandible (Dom a) where
  etaExpand (Dom ai e) = Dom ai <$> etaExpand e

instance EtaExpandible a ⇒ EtaExpandible [a] where
  etaExpand = mapM etaExpand
