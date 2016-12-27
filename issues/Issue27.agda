
module Issue27 where

infix  4 _≡_
infixl 4 _>_ _<_
infixr 1 _∨_

postulate ℝ : Set

------------------------------------------------------------------------------
-- Logic stuff

-- The identity type on the universe of discourse.
data _≡_ (x : ℝ) : ℝ → Set where
  refl : x ≡ x

data _∨_ (A B : Set) : Set where
  inj₁ : A → A ∨ B
  inj₂ : B → A ∨ B

case : ∀ {A B} → {C : Set} → (A → C) → (B → C) → A ∨ B → C
case f g (inj₁ a) = f a
case f g (inj₂ b) = g b

------------------------------------------------------------------------------
-- Real numbers stuff

postulate _>_ : ℝ → ℝ → Set

_<_ : ℝ → ℝ → Set
y < x = x > y
{-# ATP definition _<_ #-}

postulate
  trichotomy : (x y : ℝ) → (x > y) ∨ (x ≡ y) ∨ (x < y)
{-# ATP axiom trichotomy #-}

------------------------------------------------------------------------------

rmin : ℝ → ℝ → ℝ
rmin x y = case (λ _ → y) (λ h → case (λ _ → x) (λ _ → x) h) (trichotomy x y)
{-# ATP definition rmin #-}

postulate foo : ∀ x y → rmin x y ≡ rmin x y
{-# ATP prove foo #-}

{-
Error:
$ apia --check Issue27.agda
apia: tptp4X found an error/warning in the file /tmp/Issue27/44-foo.fof
Please report this as a bug

WARNING: Line 34 Char 159 Token "," : Multiple arity symbol n_62__46_8405600604384089447, arity 0 and now 2
WARNING: Line 34 Char 259 Token ")" : Multiple arity symbol n_60__48_8405600604384089447, arity 0 and now 2
WARNING: Line 34 Char 387 Token "," : Multiple arity symbol n_60__48_8405600604384089447, arity 0 and now 2
WARNING: Line 34 Char 460 Token ")" : Multiple arity symbol case_32_8405600604384089447, arity 5 and now 6
-}
