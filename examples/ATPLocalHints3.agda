-- The <local hints> in an ATP pragma <prove> can be functions.

module ATPLocalHints3 where

postulate
  D    : Set
  zero : D
  succ : D → D

data N : D → Set where
  zN :               N zero
  sN : ∀ {n} → N n → N (succ n)

1-N : N (succ zero)
1-N = sN zN

postulate
  2-N : N (succ (succ zero))
{-# ATP prove 2-N 1-N #-}
