------------------------------------------------------------------------------
-- Testing the erasing of proof terms
------------------------------------------------------------------------------

module ProofTerm1 where

postulate
  D   : Set
  N   : D → Set
  _≡_ : D → D → Set

postulate foo : ∀ {n} → (Nn : N n) → n ≡ n
{-# ATP prove foo #-}
