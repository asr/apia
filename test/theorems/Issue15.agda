{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Issue15 where

infix  7 _≡_
infixr 5 _∧_
infixr 4 _∨_

data _∨_ (A B : Set) : Set where
  inj₁ : A → A ∨ B
  inj₂ : B → A ∨ B

data _∧_ (A B : Set) : Set where
  _,_ : A → B → A ∧ B

postulate
  D    : Set
  _·_  : D → D → D
  succ : D → D
  zero : D

data ∃ (A : D → Set) : Set where
  _,_ : (t : D) → A t → ∃ A

syntax ∃ (λ x → e) = ∃[ x ] e

data _≡_ (x : D) : D → Set where
  refl : x ≡ x

postulate Conat : D → Set

postulate
  Conat-out : ∀ {n} → Conat n → n ≡ zero ∨ (∃[ n' ] n ≡ succ n' ∧ Conat n')
{-# ATP axiom Conat-out #-}

postulate
  Conat-coind :
    (A : D → Set) →
    -- A is post-fixed point of NatF.
    (∀ {n} → A n → n ≡ zero ∨ (∃[ n' ] n ≡ succ n' ∧ A n')) →
    -- Conat is greater than A.
    ∀ {n} → A n → Conat n

Conat-in : ∀ {n} →
           n ≡ zero ∨ (∃[ n' ] n ≡ succ n' ∧ Conat n') →
           Conat n
Conat-in h = Conat-coind A h' h
  where
  A : D → Set
  A n = n ≡ zero ∨ (∃[ n' ] n ≡ succ n' ∧ Conat n')
  {-# ATP definition A #-}

  postulate h' : ∀ {n} → A n → n ≡ zero ∨ (∃[ n' ] n ≡ succ n' ∧ A n')
  {-# ATP prove h' #-}
