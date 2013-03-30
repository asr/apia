------------------------------------------------------------------------------
-- Testing the translation of universal quantified propositional functions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K #-}

module NonFOL.LogicalSchemata.PropositionalFunction where

postulate D : Set

postulate id : {A : D → Set}{x : D} → A x → A x
{-# ATP prove id #-}
