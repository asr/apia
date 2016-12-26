------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Issue4 where

postulate
  D    : Set
  _≡_  : D → D → Set
  refl : ∀ {a} → a ≡ a
  P    : D → Set

-- We test the translation of a definition where we need to erase proof terms.
foo : ∀ {a} → P a → ∀ {b} → P b → a ≡ a
foo {a} Pa {b} Pb = bar
  where
  c : D
  c = a
  {-# ATP definition c #-}

  postulate bar : c ≡ a
  {-# ATP prove bar #-}
