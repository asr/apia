------------------------------------------------------------------------------
-- Testing anonymous module
------------------------------------------------------------------------------

module _ where

postulate
  D   : Set
  _≡_ : D → D → Set

postulate foo : ∀ t → t ≡ t
{-# ATP prove foo #-}
