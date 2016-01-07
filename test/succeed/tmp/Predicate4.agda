------------------------------------------------------------------------------
-- Testing many-sorted predicates inside quantifiers
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Predicate4 where

postulate
  Ty : Set
  P  : Ty → Set

postulate foo : (t : Ty) → P t → P t
{-# ATP prove foo #-}
