------------------------------------------------------------------------------
-- Testing the translation of the universal quantified formulae
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --universal-quantified-formulae #-}
{-# OPTIONS --without-K #-}

module NonFOL.LogicalSchemata.UniversalQuantifiedFormulae where

postulate D : Set

postulate id : {P : Set} → P → P
{-# ATP prove id #-}
