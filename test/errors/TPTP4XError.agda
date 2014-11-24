------------------------------------------------------------------------------
-- tptp4X yields an error because a duplicate formula
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module TPTP4XError where

postulate
  D          : Set
  true false : D
  _≡_        : D → D → Set

data Bool : D → Set where
  btrue  : Bool true
  bfalse : Bool false
{-# ATP axiom btrue false #-}

postulate foo : ∀ d → d ≡ d
{-# ATP prove foo btrue #-}
