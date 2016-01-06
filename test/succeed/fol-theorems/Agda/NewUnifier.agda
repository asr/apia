------------------------------------------------------------------------------
-- Test case due to Agda new unifier
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Agda.NewUnifier where

infixl 7 _·_
infixr 5 _∷_
infix 4  _≡_

-- The universe of discourse/universal domain.
postulate D : Set

-- The identity type on the universe of discourse.
data _≡_ (x : D) : D → Set where
  refl : x ≡ x

postulate
  _·_  : D → D → D  -- FOTC application.

-- List constants.
postulate [] cons head tail null : D  -- FOTC partial lists.

-- Definitions
abstract
  _∷_  : D → D → D
  x ∷ xs = cons · x · xs

-- The FOTC lists type (inductive predicate for total lists).
data List : D → Set where
  lnil  : List []
  lcons : ∀ x {xs} → List xs → List (x ∷ xs)
{-# ATP axioms lnil lcons #-}

postulate
  _++_  : D → D → D
  ++-[] : ∀ ys → [] ++ ys            ≡ ys
  ++-∷  : ∀ x xs ys → (x ∷ xs) ++ ys ≡ x ∷ (xs ++ ys)
{-# ATP axioms ++-[] ++-∷ #-}

++-List : ∀ {xs ys} → List xs → List ys → List (xs ++ ys)
++-List {ys = ys} lnil Lys = prf
  where postulate prf : List ([] ++ ys)
        {-# ATP prove prf #-}

++-List {ys = ys} (lcons x {xs} Lxs) Lys = prf (++-List Lxs Lys)
  where postulate prf : List (xs ++ ys) → List ((x ∷ xs) ++ ys)
        {-# ATP prove prf #-}
