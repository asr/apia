------------------------------------------------------------------------------
-- Testing the eta-expansion
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Eta2 where

postulate
  D   : Set
  ∃   : (A : D → Set) → Set
  _≡_ : D → D → Set

-- Due to eta-contraction the Agda internal representation of foo and
-- bar are the same. We eta-expand the internal types before the
-- translation to FOL.

postulate
  foo : ∀ d → ∃ (λ e → d ≡ e)
  bar : ∀ d → ∃ (_≡_ d)
{-# ATP prove foo #-}
{-# ATP prove bar #-}
