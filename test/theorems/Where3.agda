------------------------------------------------------------------------------
-- Testing the conjectures inside a @where@ clause
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Where3 where

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

-- Using @module Helper where@ instead of @where@ clauses.
+-rightIdentity : ∀ {n} → N n → n + zero ≡ n
+-rightIdentity Nn = N-ind A A0 is Nn
  module Helper where
  A : D → Set
  A i = i + zero ≡ i

  postulate A0 : zero + zero ≡ zero
  {-# ATP prove A0 #-}

  postulate is : ∀ {i} → i + zero ≡ i → succ i + zero ≡ succ i
  {-# ATP prove is #-}
