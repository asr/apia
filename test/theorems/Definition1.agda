------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

module Definition1 where

postulate
  D   : Set
  _≡_ : D → D → Set
  d   : D

-- We test the translation of the definition of a nullary function.
e : D
e = d
{-# ATP definition e #-}

postulate foo : e ≡ d
{-# ATP prove foo #-}
