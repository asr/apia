------------------------------------------------------------------------------
-- |
-- Module      : Utils.List
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Utilities on lists.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Utils.List
  ( duplicate
  , duplicatesElements
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import Data.List ( nub )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

------------------------------------------------------------------------------
-- Local imports

#include "../undefined.h"

------------------------------------------------------------------------------
-- | Return 'True' if the elements of a list occur in ascending order.
isSorted ∷ Ord a ⇒ [a] → Bool
isSorted (x : y : xs) = x <= y && isSorted (y : xs)
isSorted [_]          = True
isSorted []           = True

-- | Return 'True' if there are duplicate elements in the list.
duplicate ∷ Eq a ⇒ [a] → Bool
duplicate xs = xs /= nub xs

-- | Return the duplicates elements of an ordered list.
duplicatesElements ∷ Ord a ⇒ [a] → [a]
duplicatesElements zs =
  if isSorted zs then nub (helper zs) else __IMPOSSIBLE__
  where
  helper ∷ Eq a ⇒ [a] → [a]
  helper (x : y : xs) = if x == y then x : helper (y : xs) else helper (y : xs)
  helper [_]          = []
  helper []           = []
