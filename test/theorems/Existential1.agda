------------------------------------------------------------------------------
-- Testing the existential quantifier
------------------------------------------------------------------------------

module Existential1 where

postulate
  D   : Set
  A   : D → Set
  ∃   : (A : D → Set) → Set

postulate foo : (∃ λ x → A x) → (∃ λ x → A x)
{-# ATP prove foo #-}
