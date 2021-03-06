------------------------------------------------------------------------------
-- Testing Agda internal term: @Lam@
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- The following conjecture uses the internal Agda term @Lam@.

module Agda.InternalTerms.LamTerm where

postulate
  D : Set
  ∃ : (D → Set) → Set  -- The existential quantifier type on D.
  A : D → Set

postulate ∃-intro : (t : D) → A t → ∃ A
{-# ATP prove ∃-intro #-}
