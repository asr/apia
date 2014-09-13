------------------------------------------------------------------------------
-- Testing anonymous module
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module _ where

postulate
  D   : Set
  _≡_ : D → D → Set

postulate foo : ∀ t → t ≡ t
{-# ATP prove foo #-}
