
-- | Remove references to variables which are proof terms from Agda
-- internal types.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

------------------------------------------------------------------------------
-- General description
-- Example (Test.Succeed.Conjectures.DefinitionsInsideWhereClauses)

-- +-rightIdentity : ∀ {n} → N n → n + zero ≡ n
-- +-rightIdentity Nn = indN P P0 iStep Nn
--   where
--     P : D → Set
--     P i = i + zero ≡ i
--     {-# ATP definition P #-}

--     postulate
--       P0 : P zero
--     {-# ATP prove P0 #-}

--     postulate
--       iStep : ∀ {i} → N i → P i → P (succ i)
--     {-# ATP prove iStep #-}

-- The Agda internal type of P0 is

-- El (Type (Max []))
--    (Pi r{El (Type (Max [])) (Def Test.Succeed.Conjectures.DefinitionsInsideWhereClauses.D [])}
--        (Abs ".n" El (Type (Max []))
--                     (Pi r(El (Type (Max [])) (Def Test.Succeed.Conjectures.DefinitionsInsideWhereClauses.N [r(Var 0 [])]))
--                         (Abs "Nn" El (Type (Max []))
--                                      (Def Test.Succeed.Conjectures.DefinitionsInsideWhereClauses._.P [r{Var 1 []},r(Var 0 []),r(Def Test.Succeed.Conjectures.DefinitionsInsideWhereClauses.zero [])])))))

-- The variable Nn is a proof term (i.e. Nn : N n) and it is referenced in

-- Def Test.Succeed.Conjectures.DefinitionsInsideWhereClauses._.P [r{Var 1 []},r(Var 0 [])...       (1)

-- using its de Brujin name, i.e. r(Var 0 []). After remove this
-- reference the internal term (1) is converted to

-- Test.Succeed.Conjectures.DefinitionsInsideWhereClauses._.P [r{Var 1 []}...].

-- In addition the quantification on Nn will be removed too. See
-- Logic.Translation.Internal.Terms.agdaTermToFormula (on Pi terms).

-- End general description.

------------------------------------------------------------------------------
module Apia.Utils.AgdaAPI.RemoveProofTerms ( removeProofTerm ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Common ( Arg(Arg), Dom(Dom), Nat )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Elim'(Apply)
  , Elims
  , Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Term(Def, Lam, Pi, Sort, Var)
  , Type
  , Type'(El)
  , var
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Monad.Base
  ( getTVars
  , popTVar
  , pushTVar
  , T
  , tError
  , TError(RemoveProofTermError, TranslationOfWildCardPatterns)
  )

import Apia.Monad.Reports ( reportSLn )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Remove the reference to a variable (i.e. Var n elims) in an Agda entity.
class RemoveVar a where
  removeVar ∷ a → String → T a

instance RemoveVar Type where
  removeVar (El ty@(Type _) term) x = El ty <$> removeVar term x
  removeVar _                     _ = __IMPOSSIBLE__

instance RemoveVar Term where
  removeVar (Def qname elims) x = Def qname <$> removeVar elims x

  removeVar (Lam h (Abs y absTerm)) x = do
    pushTVar y

    reportSLn "removeVar" 20 $ "Pushed variable " ++ show y

    auxTerm ← removeVar absTerm x

    popTVar

    reportSLn "removePT" 20 $ "Pop variable " ++ show y

    return $ Lam h (Abs y auxTerm)

  -- N.B. The variables *are not* removed from the (Arg Type), they
  -- are only removed from the (Abs Type).
  removeVar (Pi domTy (Abs y absTy)) x = do

    pushTVar y
    reportSLn "removeVar" 20 $ "Pushed variable " ++ show y

    -- If the Pi term is on a proof term, we replace it by a Pi term
    -- which is not a proof term.
    newTerm ← if y /= x
                then Pi domTy . Abs y <$> removeVar absTy x
                else
                  -- We use "_" because Agda uses it.
                  Pi domTy . NoAbs "_" <$> removeVar absTy x

    popTVar
    reportSLn "removePT" 20 $ "Pop variable " ++ show y
    return newTerm

  -- N.B. The variables *are* removed from the (Arg Type).
  removeVar (Pi domTy (NoAbs y absTy)) x = do
    newArgTy ← removeVar domTy x
    newAbsTy ← removeVar absTy x
    return $ Pi newArgTy (NoAbs y newAbsTy)

  removeVar term _ = do
    reportSLn "removeVar" 20 $ "The term is: " ++ show term
    __IMPOSSIBLE__

instance RemoveVar a ⇒ RemoveVar (Dom a) where
  removeVar (Dom ai e) x = Dom ai <$> removeVar e x

-- In the Agda source code (Agda.Syntax.Internal) we have
-- type Elims = [Elim], however we cannot create the instance of Elims
-- based on a map, because in some cases we need to erase the term.

-- Requires TypeSynonymInstances and FlexibleInstances.
instance RemoveVar Elims where
  removeVar [] _ = return []

  removeVar (Apply (Arg info term@(Var n [])) : elims) x = do
    when (n < 0) (__IMPOSSIBLE__)
    vars ← getTVars

    -- TODO (2017-01-03): Missing error from the test-suite.
    when (x == "_") $
      tError TranslationOfWildCardPatterns

    let index ∷ Nat
        index = fromMaybe (__IMPOSSIBLE__) $ elemIndex x vars

    if n == index
      then removeVar elims x
      else if n < index
             then fmap ((:) (Apply (Arg info term))) (removeVar elims x)
             else fmap ((:) (Apply (Arg info (var (n - 1))))) (removeVar elims x)

  removeVar (Apply (Arg _ (Var _ _)) : _) _ = __IMPOSSIBLE__

  removeVar (Apply (Arg info term) : elims) x =
    liftM2 (\t es → Apply (Arg info t) : es) (removeVar term x) (removeVar elims x)

  removeVar _ _ = __IMPOSSIBLE__

-- | Remove a proof term from an Agda 'Type'.
removeProofTerm ∷ Type → (String, Type) → T Type
removeProofTerm ty (x, typeVar) = do
  reportSLn "removePT" 20 $
    "It is necessary to remove the variable " ++ show x ++ "?"

  case typeVar of
    -- The variable's type is a Set, e.g. the variable is d : D, where
    -- D : Set, so we don't do anything.

    -- N.B. the pattern matching on @(Def _ [])@.
    El (Type (Max [])) (Def _ []) → return ty

    -- The variable's type is a proof, e.g. the variable is Nn : N n
    -- where D : Set, n : D and N : D → Set. In this case, we remove
    -- the reference to this variable.

    -- N.B. the pattern matching on @(Def _ _)@.

    El (Type (Max [])) (Def _ _) → removeVar ty x

    -- The variable's type is a function type, i.e. Pi _ (NoAbs _ _ ),
    -- e.g. the variable is f : D → D, where D : Set.

    -- -- In the class TypesOfVar we associated to the variables bounded
    -- -- in Lam terms the type DontCare.
    -- El (Type (Max [])) DontCare → return ty

    -- Because the variable is not a proof term we don't do anything.
    El (Type (Max []))
       (Pi (Dom _ (El (Type (Max [])) (Def _ [])))
           (NoAbs _ (El (Type (Max [])) (Def _ [])))) → return ty

    -- The next case is just a generalization to various arguments of
    -- the previous case.

    -- The variable's type is a function type,
    --
    -- e.g. the variable is @f : D → D → D@, where @D : Set@.

    -- Because the variable is not a proof term we don't do anything.
    El (Type (Max []))
       (Pi (Dom _ (El (Type (Max [])) (Def _ [])))
           (NoAbs _ (El (Type (Max [])) (Pi _ (NoAbs _ _))))) →
       __IMPOSSIBLE__ -- return ty

    -- We don't erase these proofs terms.
    El (Type (Max [])) someTerm → do
      reportSLn "removePT" 20 $
                "The term someTerm is: " ++ show someTerm
      tError $ RemoveProofTermError someTerm

    -- N.B. The next case is just a generalization to various
    -- arguments of the previous case.

    -- The variable's type is @Set₁@,
    --
    -- e.g. the variable is @P : D → Set@.
    --
    -- Because the variable is not a proof term we don't do anything.
    El (Type (Max [ClosedLevel 1])) (Pi _ (NoAbs _ _)) → return ty

    -- The variable's type is @Set₁@,
    --
    -- e.g. the variable is @P : Set@.
    --
    -- Because the variable is not a proof term we don't do anything.
    El (Type (Max [ClosedLevel 1])) (Sort _) → return ty

    someType → do
      reportSLn "removePT" 20 $
                "The type varType is: " ++ show someType
      __IMPOSSIBLE__
