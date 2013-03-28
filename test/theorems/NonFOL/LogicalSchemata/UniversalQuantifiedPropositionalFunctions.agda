------------------------------------------------------------------------------
-- Testing the translation of universal quantified propositional functions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --universal-quantified-propositional-functions #-}
{-# OPTIONS --without-K #-}

module NonFOL.LogicalSchemata.UniversalQuantifiedPropositionalFunctions where

postulate D : Set

postulate id : {A : D → Set}{x : D} → A x → A x
{-# ATP prove id #-}
