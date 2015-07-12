-- The <local hints> in an ATP pragma <prove> can be postulates.

module ATPLocalHints2 where


postulate
  D    : Set
  zero : D
  succ : D → D
  N    : D → Set
  zN   : N zero
  sN   : ∀ {n} → N n → N (succ n)

postulate
  0-N : N zero
{-# ATP prove 0-N zN #-}
