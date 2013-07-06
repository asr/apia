------------------------------------------------------------------------------
-- Testing the existential quantifier
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K #-}

module Existential2 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (B : D → Set) → ∃ B → ∃ B
{-# ATP prove foo #-}
