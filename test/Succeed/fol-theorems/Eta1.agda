------------------------------------------------------------------------------
-- Testing the η-expansion
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Eta1 where

postulate
  D : Set
  A : D → Set
  ∃ : (A : D → Set) → Set

-- Due to η-contraction the Agda internal representation of foo and
-- bar are the same. We η-expand the internal types before the
-- translation to FOL.

postulate
  foo : ∀ d → A d → ∃ (λ e → A e)
  bar : ∀ d → A d → ∃ A
{-# ATP prove foo #-}
{-# ATP prove bar #-}
