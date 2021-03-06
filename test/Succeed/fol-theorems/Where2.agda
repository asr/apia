------------------------------------------------------------------------------
-- Testing the conjectures inside a @where@ clause
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Where2 where

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
{-# ATP axioms +-0x +-Sx #-}

+-assoc : ∀ {m n o} → N m → N n → N o → m + n + o ≡ m + (n + o)
+-assoc {n = n} {o} Nm Nn No = N-ind A A0 is Nm
  where
  A : D → Set
  A i = i + n + o ≡ i + (n + o)

  postulate A0 : zero + n + o ≡ zero + (n + o)
  {-# ATP prove A0 #-}

  postulate is : ∀ {i} → i + n + o ≡ i + (n + o) →
                 succ i + n + o ≡ succ i + (n + o)
  {-# ATP prove is #-}
