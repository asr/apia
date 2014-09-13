------------------------------------------------------------------------------
-- Testing the translation of universal quantified propositional functions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism          #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K                         #-}

module NonFOL.LogicalSchemata.PropositionalFunction where

postulate D : Set

postulate id₁ : {A : D → Set}{x : D} → A x → A x
{-# ATP prove id₁ #-}

postulate id₂ : {A : D → D → Set}{x y : D} → A x y → A x y
{-# ATP prove id₂ #-}
