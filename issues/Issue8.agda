{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module Issue8 where

postulate
  D : Set
  ∃ : (A : D → Set) → Set

postulate foo : (A : D → Set) → ∃ A → ∃ A
{-# ATP prove foo #-}

-- 25 February 2013: The issue is due to the η-expansion.
-- $ agda2atp -v 20 Issue8.agda
--- ...
--- The type and the eta-expanded type: equals
