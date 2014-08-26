------------------------------------------------------------------------------
-- The predicate names are translated as constant names
------------------------------------------------------------------------------

module PredicateNameConstantName where

postulate
  D : Set
  P : D → Set

postulate foo : ∀ d → P d → P d
{-# ATP prove foo #-}
