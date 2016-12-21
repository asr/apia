------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Definition07a where

open import Common.FOL

postulate
  P : D → Set

-- We test the translation of a definition where we need to erase
-- proof terms.
foo : ∀ {a} → P a → a ≡ a
foo {a} Pa = bar
  where
  c : D
  c = a
  {-# ATP definition c #-}

  postulate bar : c ≡ a
  {-# ATP prove bar #-}
