-- The ATP pragma with the role <prove> can be used with postulates.

module ATPConjecture where

postulate
  D   : Set
  _≡_ : D → D → Set

postulate
  sym : ∀ {m n} → m ≡ n → n ≡ m
{-# ATP prove sym #-}
