------------------------------------------------------------------------------
-- Testing the Agda pragma --schematic-propositional-symbols
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- Fails because requires the above pragma.

module AgdaPragmaRequired.SchematicPropositionalSymbols where

postulate D : Set

postulate id : (P : Set) → P → P
{-# ATP prove id #-}
