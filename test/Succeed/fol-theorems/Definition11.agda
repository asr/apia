------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Definition11 where

open import Common.FOL

-- We test the translation of a definition which Agda η-reduces.

P : D → Set
P d = ∃ λ e → d ≡ e
{-# ATP definition P #-}

postulate bar : ∀ {d} → P d → ∃ λ e → e ≡ d
{-# ATP prove bar #-}
