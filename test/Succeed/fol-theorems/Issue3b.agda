{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Issue3b where

module A where
  postulate
    D    : Set
    _≡_  : D → D → Set
    a b  : D

  postulate p : a ≡ b

module B where
  open A
  {-# ATP axiom p #-}

open A

postulate foo : a ≡ b
{-# ATP prove foo #-}
