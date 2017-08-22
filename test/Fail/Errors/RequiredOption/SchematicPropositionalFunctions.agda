------------------------------------------------------------------------------
-- Testing the --schematic-propositional-functions option
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- Fails because requires the above options.

module RequiredOption.SchematicPropositionalFunctions where

postulate D : Set

postulate id : (P : D → Set)(x : D) → P x → P x
{-# ATP prove id #-}
