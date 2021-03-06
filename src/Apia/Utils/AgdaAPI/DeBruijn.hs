
-- | Functions on de Bruijn indexes.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE UnicodeSyntax     #-}

-- There are various cases (e.g. η-expansion, translation of symbols'
-- definitions, elimination of quantification on variables which are
-- proof terms) where it is necessary modify the variables in the Agda
-- internal terms, i.e. it is necessary to modify the Bruijn index in
-- the variable.

module Apia.Utils.AgdaAPI.DeBruijn
  ( ChangeIndex(changeIndex)
  , DecIndex(decIndex)
  , IncIndex(incIndex)
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Common ( Arg(Arg), Dom(Dom), Nat )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Elim
  , Elim'(Apply, IApply, Proj)
  , Elims
  , Level(Max)
  , Sort(Type)
  , Tele(EmptyTel, ExtendTel)
  , Term(Con, Def, DontCare, Lam, Level, Lit, MetaV, Pi, Sort, Var)
  , Type
  , Type'(El)
  , var
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )

#include "undefined.h"

------------------------------------------------------------------------------
-- | To increase by one the de Bruijn index of the variable in an Agda
-- entity.
class IncIndex a where
  incIndex ∷ a → a

instance IncIndex Term where
  incIndex term@(Def _ _) = term

  incIndex (Var n []) | n >= 0    = var (n + 1)
                      | otherwise = __IMPOSSIBLE__

  incIndex Con{}        = __IMPOSSIBLE__
  incIndex (DontCare _) = __IMPOSSIBLE__
  incIndex (Lam _ _ )   = __IMPOSSIBLE__
  incIndex (Level _)    = __IMPOSSIBLE__
  incIndex (Lit _)      = __IMPOSSIBLE__
  incIndex (MetaV _ _)  = __IMPOSSIBLE__
  incIndex (Pi _ _)     = __IMPOSSIBLE__
  incIndex (Sort _)     = __IMPOSSIBLE__
  incIndex (Var _ _)    = __IMPOSSIBLE__

instance IncIndex Elim where
  incIndex (Apply (Arg color term)) = Apply (Arg color $ incIndex term)
  incIndex (Proj _ _)               = __IMPOSSIBLE__
  incIndex IApply{}                 = __IMPOSSIBLE__

instance IncIndex a ⇒ IncIndex [a] where
  incIndex = map incIndex

------------------------------------------------------------------------------
-- | To decrease by one the de Bruijn index of the variable in an Agda
-- entity.
class DecIndex a where
  decIndex ∷ a → a

instance DecIndex Term where
  decIndex (Def qname elims) = Def qname $ decIndex elims
  decIndex term@(Var 0 []) = term
  decIndex (Var n []) | n > 0     = var (n - 1)
                      | otherwise = __IMPOSSIBLE__
  decIndex (Var _ _) = __IMPOSSIBLE__
  decIndex _ = __IMPOSSIBLE__

instance DecIndex Type where
  decIndex (El (Type (Max [])) term) = El (Type (Max [])) (decIndex term)
  decIndex _                         = __IMPOSSIBLE__

instance DecIndex Elim where
  decIndex (Apply (Arg color term)) = Apply (Arg color $ decIndex term)
  decIndex (Proj _ _)               = __IMPOSSIBLE__
  decIndex IApply{}                 = __IMPOSSIBLE__

instance DecIndex a ⇒ DecIndex [a] where
  decIndex = map decIndex

instance DecIndex a ⇒ DecIndex (Dom a) where
  decIndex (Dom ai b e) = Dom ai b $ decIndex e

instance DecIndex a ⇒ DecIndex (Abs a) where
  decIndex (Abs name body) = Abs name $ decIndex body
  decIndex (NoAbs _ _)     = __IMPOSSIBLE__

instance DecIndex a ⇒ DecIndex (Tele a) where
  decIndex EmptyTel          = EmptyTel
  decIndex (ExtendTel a tel) = ExtendTel (decIndex a) (decIndex tel)

------------------------------------------------------------------------------
-- We collect the variables' names using the type class VarNames. The
-- de Bruijn indexes are assigned from right to left,
--
-- e.g.  in @(A B C : Set) → ...@, @A@ is 2, @B@ is 1, and @C@ is 0,
--
-- so we need create the list in the same order.

class VarNames a where
  varNames ∷ a → [String]

instance VarNames Term where
  varNames (Def _ elims) = varNames elims

  varNames (Lam _ (Abs x term')) = varNames term' ++ [x]

  varNames (Var n [])
    | n >= 0    = []
    | otherwise = __IMPOSSIBLE__

  -- 31 May 2012. We don't have an example of this case.
  --
  -- Var _ args → varNames args
  varNames (Var _ _) = __IMPOSSIBLE__

  varNames _ = __IMPOSSIBLE__

instance VarNames Elim where
  varNames (Apply (Arg _ term)) = varNames term
  varNames (Proj _ _)           = __IMPOSSIBLE__
  varNames IApply{}             = __IMPOSSIBLE__

instance VarNames a ⇒ VarNames [a] where
  varNames = concatMap varNames

------------------------------------------------------------------------------
-- | To change a de Bruijn index with respect to other index in an
-- Agda entity.

-- Let's suppose we have something like

-- @λ m : D → (λ n : D → (λ Nn : N n → (λ h : D → ... Var 2 ...)))@

-- where @Var 2@ is the de Bruijn index of the variable @n@. If we
-- drop the quantification on the proof term @Nn@

-- @λ m : D → (λ n : D → (λ h : D → ...))@

-- we need change @Var 2@ by @Var 1@.

class ChangeIndex a where
  changeIndex ∷ a → Nat → a

instance ChangeIndex Term where
  changeIndex term'@(Def _ []) _ = term'

  changeIndex (Def qName elims) index = Def qName $ changeIndex elims index

  changeIndex (Lam h (Abs x term')) index =
    Lam h (Abs x (changeIndex term' index))

  -- When the variable is part of an argument, it was processed in
  -- the Args instance.
  changeIndex (Var n []) index
    | n < 0 = __IMPOSSIBLE__
    -- The variable was after than the quantified variable, we need
    -- "unbound" the quantified variable.
    | n > index = var (n - 1)

    -- In the case @n < index@ the variable was before than the
    -- quantified variable, therefore we shouldn't do nothing,
    -- i.e. we should return the term.
    | otherwise = __IMPOSSIBLE__

  changeIndex _ _ = __IMPOSSIBLE__

-- In the Agda source code (Agda.Syntax.Internal) we have
-- type Elims = [Elim], however we cannot create the instance of Elims
-- based on a map, because in some cases we need to erase the term.

-- Requires TypeSynonymInstances and FlexibleInstances.
instance ChangeIndex Elims where
  changeIndex [] _ = []

  changeIndex (Apply (Arg info term@(Var n [])) : elims) index
    -- The variable was before than the quantified variable, we don't
    -- do nothing.
    | n < index = Apply (Arg info term) : changeIndex elims index

    -- The variable was after than the quantified variable, we need
    -- "unbound" the quantified variable.
    | n > index = Apply (Arg info (var (n - 1))) : changeIndex elims index

    -- The variable is the quantified variable. This can happen when
    -- the quantified variable is used indirectly by other term via
    -- for example a where clause (see for example xxx). In this case,
    -- we drop the variable. Before this modification, we returned
    -- __IMPOSSIBLE__.
    | n == index = changeIndex elims index

  changeIndex (Apply (Arg _ (Var _ _)) : _) _ = __IMPOSSIBLE__

  changeIndex (Apply (Arg info term) : elims) index =
    Apply (Arg info (changeIndex term index)) : changeIndex elims index

  changeIndex _ _ = __IMPOSSIBLE__
