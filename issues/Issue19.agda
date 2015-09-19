------------------------------------------------------------------------------
-- Testing HOL translation
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Issue19 where

postulate
  Ty  : Set
  _≡_ : {A : Set} → A → A → Set

postulate foo : (t : Ty) → t ≡ t
{-# ATP prove foo #-}
