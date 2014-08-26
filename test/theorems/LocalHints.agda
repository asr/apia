------------------------------------------------------------------------------
-- Testing the use of local hints
------------------------------------------------------------------------------

module LocalHints where

postulate
  D    : Set
  zero : D

data N : D → Set where
  zN : N zero

postulate 0-N : N zero
{-# ATP prove 0-N zN #-}
