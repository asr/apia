------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

module Definition2 where

postulate
  D   : Set
  _≡_ : D → D → Set

-- We test the translation of the definition of a unary function.
foo : D → D
foo d = d
{-# ATP definition foo #-}

postulate bar : ∀ d → d ≡ foo d
{-# ATP prove bar #-}
