------------------------------------------------------------------------------
-- Testing the translation of the universal quantified propositional symbols
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-symbols #-}

module NonFOL.LogicalSchemata.PropositionalSymbol where

postulate D : Set

postulate id : {P : Set} → P → P
{-# ATP prove id #-}
