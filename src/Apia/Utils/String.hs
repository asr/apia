------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.String
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities on strings.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.String
  ( removeString
  , toUpperFirst
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Text.Regex ( mkRegex, subRegex )

#include "undefined.h"

------------------------------------------------------------------------------
-- | @removeString xs ys@ removes every occurrence of @xs@ in @ys@.
removeString ∷ String → String → String
removeString repl inp = subRegex (mkRegex repl) inp ""

-- | Convert the first letter of a string to the corresponding
-- upper-case letter.
toUpperFirst ∷ String → String
toUpperFirst []       = __IMPOSSIBLE__
toUpperFirst (x : xs) =  toUpper x : xs
