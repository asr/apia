-- The <local hints> in an ATP pragma <prove> can be data constructors.

module ATPLocalHints1 where

postulate
  D    : Set
  zero : D
  succ : D → D

data N : D → Set where
  zN :               N zero
  sN : ∀ {n} → N n → N (succ n)

postulate
  0-N : N zero
{-# ATP prove 0-N zN #-}
