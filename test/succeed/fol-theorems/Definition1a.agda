------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Definition1a where

postulate
  D   : Set
  _≡_ : D → D → Set
  d   : D

-- We test the translation of the definition of a nullary function.
e : D
e = d
{-# ATP definition e #-}

postulate foo : e ≡ d
{-# ATP prove foo #-}
