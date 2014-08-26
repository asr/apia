------------------------------------------------------------------------------
-- Testing Agda internal terms: @Var Nat Args@ when @Args = []@
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-functions #-}

module NonFOL.AgdaInternalTerms.VarEmptyArgumentsTerm where

postulate D : Set

postulate id : (P : D → Set)(x : D) → P x → P x
{-# ATP prove id #-}
