------------------------------------------------------------------------------
-- Testing Agda internal terms: @Var Nat Args@ when @Args = []@
------------------------------------------------------------------------------

{-# OPTIONS --exact-split                       #-}
{-# OPTIONS --no-sized-types                    #-}
{-# OPTIONS --no-universe-polymorphism          #-}
{-# OPTIONS --schematic-propositional-functions #-}
{-# OPTIONS --without-K                         #-}

module AgdaInternalTerms.VarEmptyArgumentsTerm where

postulate D : Set

postulate id : (P : D → Set)(x : D) → P x → P x
{-# ATP prove id #-}