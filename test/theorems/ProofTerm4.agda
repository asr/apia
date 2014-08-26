------------------------------------------------------------------------------
-- Testing the erasing of proof terms
------------------------------------------------------------------------------

module ProofTerm4 where

postulate
  D   : Set
  _≡_ : D → D → Set
  A   : D → D → D → D → Set

-- We can erase proof terms of type D → ⋯ → D → Set.
foo : ∀ x₁ x₂ x₃ x₄ → A x₁ x₂ x₃ x₄ → x₁ ≡ x₁
foo x₁ x₂ x₃ x₄ h = bar x₁
  where
  -- Since @bar@ is insidere a where clause, we erase the proof
  -- term @h@.
  postulate bar : ∀ x → x ≡ x
  {-# ATP prove bar #-}
