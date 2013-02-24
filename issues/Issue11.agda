------------------------------------------------------------------------------
-- All the arguments are required in an ATP definition
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module Issue11 where

postulate
  D          : Set
  false true : D

data Bool : D → Set where
  btrue  : Bool true
  bfalse : Bool false

BitOk : D → Set
BitOk b = Bool b
{-# ATP definition BitOk #-}

postulate foo : ∀ b → BitOk b → BitOk b
{-# ATP prove foo #-}

BitWrong : D → Set
BitWrong = Bool
{-# ATP definition BitWrong #-}

postulate bar : ∀ b → BitWrong b → BitWrong b
{-# ATP prove bar #-}
