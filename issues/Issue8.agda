{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module Issue8 where

open import Common.FOL.FOL

postulate foo : (A : D → Set) → (∃ λ x → A x) → (∃ λ x → A x)
{-# ATP prove foo #-}
