module PredicateTranslation where

postulate
  D : Set
  P : D → Set

postulate foo : ∀ d → P d → P d
{-# ATP prove foo #-}
