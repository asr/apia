------------------------------------------------------------------------------
-- Testing anonymous module
------------------------------------------------------------------------------

-- No top-level module

postulate
  D   : Set
  _≡_ : D → D → Set

postulate foo : ∀ t → t ≡ t
{-# ATP prove foo #-}
