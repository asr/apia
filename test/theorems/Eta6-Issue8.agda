------------------------------------------------------------------------------
-- Testing the eta-expansion
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-functions #-}

module Eta6-Issue8 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (A : D → Set) → ∃ A → ∃ A
{-# ATP prove foo #-}
