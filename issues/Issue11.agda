------------------------------------------------------------------------------
-- All parameters are required in an ATP definition
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Issue11 where

postulate
  D          : Set
  false true : D

data Bool : D → Set where
  btrue  : Bool true
  bfalse : Bool false

OkBit : D → Set
OkBit b = Bool b
{-# ATP definition OkBit #-}

postulate foo : ∀ b → OkBit b → OkBit b
{-# ATP prove foo #-}

WrongBit : D → Set
WrongBit = Bool
{-# ATP definition WrongBit #-}

postulate bar : ∀ b → WrongBit b → WrongBit b
{-# ATP prove bar #-}

-- $ apia --check Issue11.agda
-- apia: tptp4X found an error in the file /tmp/Issue11/29-bar.tptp
