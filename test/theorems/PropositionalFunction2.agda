------------------------------------------------------------------------------
-- Testing the translation of the propositional functions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module PropositionalFunction2 where

------------------------------------------------------------------------------

postulate
  D   : Set
  P   : D → Set
  a   : D
  _∨_ : Set → Set → Set

A : D → Set
A x = P x ∨ P x
{-# ATP definition A #-}

postulate foo : A a → A a
{-# ATP prove foo #-}
