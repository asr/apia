------------------------------------------------------------------------------
-- Incompatible options
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- The @--schematic-propositional-functions@ and
-- @--without-predicate-symbols@ options are incompatible.

module SchematicPropositionalFunctionsWithoutPredicateSymbols where

postulate D : Set

postulate id : {P : D → Set}{x : D} → P x → P x
{-# ATP prove id #-}
