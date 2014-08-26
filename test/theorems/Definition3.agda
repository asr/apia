------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

module Definition3 where

postulate
  D   : Set
  _≡_ : D → D → Set

-- We test the translation of the definition of a binary function.
foo : D → D → D
foo d e = d
{-# ATP definition foo #-}

postulate bar : ∀ d e → d ≡ foo d e
{-# ATP prove bar #-}
