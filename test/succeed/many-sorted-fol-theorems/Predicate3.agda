------------------------------------------------------------------------------
-- Testing many-sorted predicates
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Predicate3 where

postulate
  Tx Ty : Set
  P     : Tx → Set
  Q     : Ty → Set
  a     : Tx
  b     : Ty

postulate foo : (P a → Q b) → P a → Q b
{-# ATP prove foo #-}
