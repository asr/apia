------------------------------------------------------------------------------
-- Testing the erasing of proof terms
------------------------------------------------------------------------------

module ProofTerm2 where

postulate
  D   : Set
  N   : D → Set
  _≡_ : D → D → Set

postulate foo : ∀ {m n} → (Nm : N m) → (Nn : N n) → m ≡ m
{-# ATP prove foo #-}
