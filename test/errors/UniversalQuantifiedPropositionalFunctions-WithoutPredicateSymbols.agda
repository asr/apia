------------------------------------------------------------------------------
-- Incompatible options
------------------------------------------------------------------------------

{-# OPTIONS --universal-quantified-propositional-functions #-}

-- The options @--universal-quantified-propositional-functions@ and
-- @--without-predicate-symbols<@ are incompatible.

module UniversalQuantifiedPropositionalFunctions-WithoutPredicateSymbols where

postulate D : Set

postulate id : {P : D → Set}{x : D} → P x → P x
{-# ATP prove id #-}
