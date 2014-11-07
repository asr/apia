------------------------------------------------------------------------------
-- Testing anonymous module
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- No top-level module

postulate
  D   : Set
  _≡_ : D → D → Set

postulate foo : ∀ t → t ≡ t
{-# ATP prove foo #-}
