------------------------------------------------------------------------------
-- Testing the existential quantifier
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Existential where

postulate
  D   : Set
  A   : D → Set
  ∃   : (A : D → Set) → Set

postulate foo : (∃ λ x → A x) → (∃ λ x → A x)
{-# ATP prove foo #-}
