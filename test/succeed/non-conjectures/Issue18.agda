module Issue18 where

postulate
  D : Set

data ∃ (A : D → Set) : Set where
  _,_ : (witness : D) → A witness → ∃ A
