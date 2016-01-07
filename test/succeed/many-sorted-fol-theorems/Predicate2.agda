------------------------------------------------------------------------------
-- Testing many-sorted predicates
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Predicate2 where

postulate
  Ty  : Set
  P   : Ty → Ty → Set
  a b : Ty

postulate foo : P a b → P a b
{-# ATP prove foo #-}
