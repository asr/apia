------------------------------------------------------------------------------
-- tptp4X yields an error because a duplicate formula
------------------------------------------------------------------------------

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
