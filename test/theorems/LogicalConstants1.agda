------------------------------------------------------------------------------
-- Testing the translation of the logical constants
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

module LogicalConstants1 where

------------------------------------------------------------------------------

postulate
  A : Set

postulate foo : A → A
{-# ATP prove foo #-}
