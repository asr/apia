------------------------------------------------------------------------------
-- Testing the translation of the universal quantified propositional symbols
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module PropositionalSymbol where

postulate D : Set

postulate id : {P : Set} → P → P
{-# ATP prove id #-}
