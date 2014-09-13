{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- In the TPTP generated file, the line of the ATP definition @eq is
-- -1. It should be 16.

module Issue13 where

postulate
  D   : Set
  _≡_ : D → D → Set
  d   : D

e : D
e = d
{-# ATP definition e #-}

postulate foo : e ≡ d
{-# ATP prove foo #-}
