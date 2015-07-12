-- The ATP pragma with the role <hint> can be used with functions.

module ATPHint where

postulate
  D : Set

data _≡_ (x : D) : D → Set where
  refl : x ≡ x

sym : ∀ {m n} → m ≡ n → n ≡ m
sym refl = refl
{-# ATP hint sym #-}
