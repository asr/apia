------------------------------------------------------------------------------
-- The predicate names are translated as constant names
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module PredicateNameConstantName where

postulate
  D : Set
  P : D → Set

postulate foo : ∀ d → P d → P d
{-# ATP prove foo #-}
