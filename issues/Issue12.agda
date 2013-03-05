------------------------------------------------------------------------------
-- Issue 12
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --universal-quantified-propositional-functions #-}
{-# OPTIONS --without-K #-}

module Issue12 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (A B : D → Set) → ∃ B → ∃ B
{-# ATP prove foo #-}
