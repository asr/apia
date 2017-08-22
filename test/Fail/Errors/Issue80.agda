-- Issue #80.

-- The disjunction data type.
data _∨_ (A B : Set) : Set where
  inj₁ : A → A ∨ B
  inj₂ : B → A ∨ B

-- A different symbol for disjunction.
_⊕_ : Set → Set → Set
P ⊕ Q = P ∨ Q
{-# ATP definition _⊕_ #-}

postulate
  P Q : Set
  ⊕-comm : P ⊕ Q → Q ⊕ P
{-# ATP prove ⊕-comm #-}

-- The previous error was:

-- $ apia Issue80.agda
-- An internal error has occurred. Please report this as a bug.
-- Location of the error: src/Apia/Translation/Functions.hs:188

-- The current error is:

-- $ apia Issue80.agda
-- apia: the translation of ‘IssueXX._⊕_’ failed because it is not a FOL-definition
