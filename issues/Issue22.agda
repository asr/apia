module Issue22 where

postulate
  D   : Set
  _≡_ : D → D → Set
  d e : D

foo : ∀ x → x ≡ x
foo x = bar
  where
  postulate
    d≡e : d ≡ e

  postulate
    bar : x ≡ x
  {-# ATP prove bar d≡e #-}

-- $ apia Bug.agda
-- An internal error has occurred. Please report this as a bug.
-- Location of the error: src/Apia/Utils/AgdaAPI/Interface.hs:307

-- The error occurs because the dead code analysis removes `d≡e` from
-- the the interfase file.
