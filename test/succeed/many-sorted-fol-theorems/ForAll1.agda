------------------------------------------------------------------------------
-- Testing many-sorted for all
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module ForAll1 where

postulate
  Ty  : Set
  _≡_ : Ty → Ty → Set

postulate foo : (t : Ty) → t ≡ t
{-# ATP prove foo #-}
