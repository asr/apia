------------------------------------------------------------------------------
-- Issue in the translation
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module Issue10 where

postulate
  D  : Set
  P' : D → Set

postulate foo : ∀ d → P' d → P' d
{-# ATP prove foo #-}
