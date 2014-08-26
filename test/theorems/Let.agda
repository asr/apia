------------------------------------------------------------------------------
-- Testing let expressions
------------------------------------------------------------------------------

module Let where

postulate
  D   : Set
  _≡_ : D → D → Set

-- postulate foo : ∀ t t' → t ≡ t'  → t' ≡ t
-- {-# ATP prove foo #-}

postulate bar : ∀ t t' → let t≡t' = t ≡ t' in t≡t' → t' ≡ t
{-# ATP prove bar #-}
