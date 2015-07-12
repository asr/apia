-- The ATP pragma with the role <definition> can be used with functions.

module ATPDefinition where

postulate
  D    : Set
  zero : D
  succ : D → D

one : D
one = succ zero
{-# ATP definition one #-}
