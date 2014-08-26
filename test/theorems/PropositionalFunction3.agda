------------------------------------------------------------------------------
-- Testing the translation of the propositional functions
------------------------------------------------------------------------------

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
