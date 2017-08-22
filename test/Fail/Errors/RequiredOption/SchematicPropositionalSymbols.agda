------------------------------------------------------------------------------
-- Testing the --schematic-propositional-symbols option
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

-- Fails because requires the above option.

module RequiredOption.SchematicPropositionalSymbols where

postulate D : Set

postulate id : (P : Set) → P → P
{-# ATP prove id #-}
