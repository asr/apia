------------------------------------------------------------------------------
-- Testing the translation of the propositional functions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module PropositionalFunction3 where

------------------------------------------------------------------------------

postulate
  D   : Set
  P   : D → Set
  a   : D
  _∨_ : Set → Set → Set

-- In this case, the propositional function uses predicates and logical
-- constants, and it is an anonymous function.
postulate foo : (λ x → P x ∨ P x) a → (λ x → P x ∨ P x) a
{-# ATP prove foo #-}
