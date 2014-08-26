------------------------------------------------------------------------------
-- Testing Agda internal term: @Def@
------------------------------------------------------------------------------

module AgdaInternalTerms.DefTerm2 where

------------------------------------------------------------------------------

postulate
  D     : Set
  _≡_   : D → D → Set
  P³    : D → D → D → Set
  a b c : D

-- A definition with three arguments.
postulate foo : P³ a b c
{-# ATP axiom foo #-}

-- We need to have at least one conjecture to generate a TPTP file.
postulate bar : ∀ d → d ≡ d
{-# ATP prove bar #-}
