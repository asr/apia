------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Definition9 where

infixl 6 _+_
infix  4 _≡_

postulate
  D      : Set
  zero   : D
  succ   : D → D
  _≡_    : D → D → Set

data N : D → Set where
  nzero : N zero
  nsucc : ∀ {n} → N n → N (succ n)

N-ind : (A : D → Set) →
        A zero →
        (∀ {n} → A n → A (succ n)) →
        ∀ {n} → N n → A n
N-ind A A0 h nzero      = A0
N-ind A A0 h (nsucc Nn) = h (N-ind A A0 h Nn)

postulate
  _+_  : D → D → D
  +-0x : ∀ n → zero + n     ≡ n
  +-Sx : ∀ m n → succ m + n ≡ succ (m + n)
{-# ATP axiom +-0x +-Sx #-}

-- We test the translation of a definition where we need to erase
-- proof terms.
+-rightIdentity : ∀ {n} → N n → n + zero ≡ n
+-rightIdentity Nn = N-ind A A0 is Nn
  where
  A : D → Set
  A i = i + zero ≡ i
  {-# ATP definition A #-}

  postulate A0 : A zero
  {-# ATP prove A0 #-}

  postulate is : ∀ {i} → A i → A (succ i)
  {-# ATP prove is #-}
