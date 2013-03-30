------------------------------------------------------------------------------
-- Incompatible options
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-functions #-}

-- The options @--schematic-propositional-functions@ and
-- @--without-predicate-symbols<@ are incompatible.

module SchematicPropositionalFunctions-WithoutPredicateSymbols where

postulate D : Set

postulate id : {P : D → Set}{x : D} → P x → P x
{-# ATP prove id #-}
