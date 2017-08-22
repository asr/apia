{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

postulate
  D   : Set
  _≡_ : D → D → Set
  a b : D

postulate p : a ≡ b
{-# ATP axiom p #-}

postulate foo : a ≡ b
{-# ATP prove foo #-}
