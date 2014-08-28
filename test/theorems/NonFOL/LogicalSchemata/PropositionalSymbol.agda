------------------------------------------------------------------------------
-- Testing the translation of the universal quantified propositional symbols
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --schematic-propositional-symbols #-}
{-# OPTIONS --without-K #-}

module NonFOL.LogicalSchemata.PropositionalSymbol where

postulate D : Set

postulate id : {P : Set} → P → P
{-# ATP prove id #-}
