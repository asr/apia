------------------------------------------------------------------------------
-- No theorem used by the shelltestrunner test
------------------------------------------------------------------------------

module NoTheorem where

postulate
  D   : Set
  _≡_ : D → D → Set
  a b : D

postulate foo : a ≡ b
{-# ATP prove foo #-}
