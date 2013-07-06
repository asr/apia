------------------------------------------------------------------------------
-- Issue 12
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K #-}

module Issue12 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (B C : D → Set) → ∃ C → ∃ C
{-# ATP prove foo #-}
