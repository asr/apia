------------------------------------------------------------------------------
-- We only translate first-order definitions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module HigherOrderDefinition where

------------------------------------------------------------------------------

postulate
  D    : Set
  _≡_  : D → D → Set
  succ : D → D

-- A higher-order definition
twice : (D → D) → D → D
twice f x = f (f x)
{-# ATP definition twice #-}

postulate twice-succ : ∀ n → twice succ n ≡ succ (succ n)
{-# ATP prove twice-succ #-}
