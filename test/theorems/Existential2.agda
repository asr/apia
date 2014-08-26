------------------------------------------------------------------------------
-- Testing the existential quantifier
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-functions #-}

module Existential2 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (B : D → Set) → ∃ B → ∃ B
{-# ATP prove foo #-}
