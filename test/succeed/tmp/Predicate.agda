------------------------------------------------------------------------------
-- Testing many-sorted predicates
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Predicate where

postulate
  Ty : Set
  P  : Ty → Set
  a  : Ty

postulate foo : P a → P a
{-# ATP prove foo #-}
