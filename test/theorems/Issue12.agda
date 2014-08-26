------------------------------------------------------------------------------
-- Issue 12
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-functions #-}

module Issue12 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (B C : D → Set) → ∃ C → ∃ C
{-# ATP prove foo #-}

postulate bar : (B C : D → Set) → ∃ B → ∃ B
{-# ATP prove bar #-}
