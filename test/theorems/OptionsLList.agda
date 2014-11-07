------------------------------------------------------------------------------
-- Testing the use of various options using only an OPTIONS pragma
------------------------------------------------------------------------------

{-# OPTIONS --exact-split
            --no-universe-polymorphism
            --schematic-propositional-functions
            --without-K
#-}

-- (24 January 2014) From the Agda implementation, the above list of
-- options is saved in the interface file as an one element list,
-- where the element is a list of the three options.

module OptionsLList where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (A : D → Set) → ∃ A → ∃ A
{-# ATP prove foo #-}
