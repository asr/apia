
postulate
  D  : Set
  ∃  : (A : D → Set) → Set
  vx : D
  Q₂ : D → D → Set

postulate
  foo : (∃ λ x → Q₂ vx x) → (∃ λ x → Q₂ vx x)
{-# ATP prove foo #-}

-- $ apia Bug.agda
-- An internal error has occurred. Please report this as a bug.
-- Location of the error: src/Apia/Utils/AgdaAPI/DeBruijn.hs:68
