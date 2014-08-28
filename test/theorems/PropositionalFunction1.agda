------------------------------------------------------------------------------
-- Testing the translation of the propositional functions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module PropositionalFunction1 where

------------------------------------------------------------------------------

postulate
  D  : Set
  A  : D → Set
  a  : D

-- In this case, the propositional function does not use logical
-- constants.
postulate foo : A a → A a
{-# ATP prove foo #-}
