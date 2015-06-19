------------------------------------------------------------------------------
-- Testing the η-expansion
------------------------------------------------------------------------------

{-# OPTIONS --exact-split                       #-}
{-# OPTIONS --no-sized-types                    #-}
{-# OPTIONS --no-universe-polymorphism          #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K                         #-}

module Eta6-Issue8 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (A : D → Set) → ∃ A → ∃ A
{-# ATP prove foo #-}
