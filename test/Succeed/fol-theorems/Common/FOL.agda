------------------------------------------------------------------------------
-- Common FOL definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Common.FOL where

infix  6 ∃
infix  4 _≡_
infixr 4 _,_

postulate D : Set

-- The existential quantifier type on D.
data ∃ (A : D → Set) : Set where
  _,_ : (t : D) → A t → ∃ A

-- The identity type on D.
data _≡_ (x : D) : D → Set where
  refl : x ≡ x
