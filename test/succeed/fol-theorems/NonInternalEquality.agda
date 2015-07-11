------------------------------------------------------------------------------
-- Testing the non-internal ATP equality
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module NonInternalEquality where

postulate D : Set

data _≡_ (x : D) : D → Set where
  refl : x ≡ x
{-# ATP axiom refl #-}

postulate foo : ∀ x → x ≡ x
{-# ATP prove foo #-}
