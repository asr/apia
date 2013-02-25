{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module Issue8 where

postulate D : Set

-- The existential quantifier type on D.
data ∃ (A : D → Set) : Set where
  _,_ : (t : D) → A t → ∃ A

postulate foo : (A : D → Set) → (∃ λ x → A x) → (∃ λ x → A x)
{-# ATP prove foo #-}
