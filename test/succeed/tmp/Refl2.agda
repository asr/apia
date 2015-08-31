------------------------------------------------------------------------------
-- Testing many-sorted for all
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Refl2 where

infixr 2 _∧_
infix  4 _≡_

postulate
  Ty  : Set
  _≡_ :  Ty → Ty → Set
  _∧_ : Set → Set → Set

postulate foo : (t t' : Ty) → t ≡ t ∧ t' ≡ t'
{-# ATP prove foo #-}
