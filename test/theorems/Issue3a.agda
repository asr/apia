module Issue3a where

module A where
  postulate
    D    : Set
    _≡_  : D → D → Set
    a b  : D

  postulate p : a ≡ b

open A
{-# ATP axiom p #-}

postulate foo : a ≡ b
{-# ATP prove foo #-}
