------------------------------------------------------------------------------
-- Testing the Agda pragma --schematic-propositional-functions
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

-- Fails because requires the above pragma.

module AgdaPragmaRequired.SchematicPropositionalFunctions where

postulate D : Set

postulate id : (P : D → Set)(x : D) → P x → P x
{-# ATP prove id #-}
