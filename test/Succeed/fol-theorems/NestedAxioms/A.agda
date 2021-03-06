------------------------------------------------------------------------------
-- Testing nested axioms
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module NestedAxioms.A where

------------------------------------------------------------------------------

postulate
  D   : Set
  _≡_ : D → D → Set

postulate
  a b : D
  a≡b : a ≡ b
{-# ATP axiom a≡b #-}
