------------------------------------------------------------------------------
-- Testing many-sorted for all
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module ForAll where

postulate
  Ty : Set
  P  : Ty → Set

postulate foo : (t : Ty) → P t → P t
{-# ATP prove foo #-}
