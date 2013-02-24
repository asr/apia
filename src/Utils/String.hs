------------------------------------------------------------------------------
-- |
-- Module      : Utils.String
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Utilities on strings.
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Utils.String ( removeString ) where

------------------------------------------------------------------------------
-- Haskell imports

import Text.Regex ( mkRegex, subRegex )

------------------------------------------------------------------------------
-- | @removeString xs ys@ removes every occurrence of @xs@ in @ys@.
removeString ∷ String → String → String
removeString repl inp = subRegex (mkRegex repl) inp ""
