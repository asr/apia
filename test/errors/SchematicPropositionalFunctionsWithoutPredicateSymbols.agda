------------------------------------------------------------------------------
-- Incompatible options
------------------------------------------------------------------------------

{-# OPTIONS --exact-split                       #-}
{-# OPTIONS --no-sized-types                    #-}
{-# OPTIONS --no-universe-polymorphism          #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K                         #-}

-- The Agda @--schematic-propositional-functions@ option and the Apia
-- @--without-predicate-symbols@ option are incompatible.

module SchematicPropositionalFunctionsWithoutPredicateSymbols where

postulate D : Set

postulate id : {P : D → Set}{x : D} → P x → P x
{-# ATP prove id #-}
