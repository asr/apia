------------------------------------------------------------------------------
-- Testing various arguments in the ATP pragmas
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module VariousArguments where

postulate
  D       : Set
  _≡_     : D → D → Set
  a b c d : D
  a≡b     : a ≡ b
  b≡c     : b ≡ c
  c≡d     : c ≡ d
{-# ATP axioms a≡b b≡c c≡d #-}

postulate a≡d : a ≡ d
{-# ATP prove a≡d #-}
