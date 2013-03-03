{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module Issue8 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (A : D → Set) → ∃ A
{-# ATP prove foo #-}
