-- The ATP pragma with the role <axiom> can be used with data constructors.

module ATPAxiomDataConstructors where

postulate
  D    : Set
  zero : D
  succ : D → D

data N : D → Set where
  zN :               N zero
  sN : ∀ {n} → N n → N (succ n)
{-# ATP axiom zN #-}
