------------------------------------------------------------------------------
-- Testing many-sorted for all
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Refl2Types where

infixr 2 _∧_
infix  4 _≡_

postulate
  Tx Ty : Set
  _≡_   : {A : Set} → A → A → Set
  _∧_   : Set → Set → Set

postulate foo : (x : Tx)(y : Ty) → x ≡ x ∧ y ≡ y
{-# ATP prove foo #-}
