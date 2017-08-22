module Issue23 where

postulate
  ℝ   : Set
  r₀  : ℝ
  _+_ : ℝ → ℝ → ℝ
  -_  : ℝ → ℝ

_-_ : ℝ → ℝ → ℝ
x - y = x + (- y)

infixl 4 _≡_

-- Equality.
data _≡_ : ℝ → ℝ → Set where
  refl : (x : ℝ) → x ≡ x

postulate ─-neut : {x : ℝ} → r₀ - x ≡ - x
{-# ATP prove ─-neut #-}
