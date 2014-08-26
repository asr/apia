------------------------------------------------------------------------------
-- Testing the translation of the propositional functions
------------------------------------------------------------------------------

module PropositionalFunction4 where

------------------------------------------------------------------------------

postulate
  D   : Set
  f   : D → D
  a   : D
  _≡_ : D → D → Set

A : D → Set
A x = f x ≡ f x
{-# ATP definition A #-}

-- In this case the propositional function uses functions.
postulate foo : A a → A a
{-# ATP prove foo #-}
