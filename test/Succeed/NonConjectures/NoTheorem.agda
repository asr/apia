------------------------------------------------------------------------------
-- No theorem used by the shelltestrunner test
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module NoTheorem where

postulate
  D   : Set
  _≡_ : D → D → Set
  a b : D

postulate foo : a ≡ b
{-# ATP prove foo #-}
