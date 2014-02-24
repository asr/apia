------------------------------------------------------------------------------
-- |
-- Module      : Utils.Show
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities related to 'Show'.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Utils.Show
  ( showListLn
  , showLn
  ) where

------------------------------------------------------------------------------
-- | Version of 'show' adding a newline character.
showLn ∷ Show a ⇒ a → String
showLn = (++ "\n") . show

-- | Version of 'show' on lists where the elements are separated by
-- newline characters.
showListLn ∷ Show a ⇒ [a] → String
showListLn [] = "[]"
showListLn xs = concatMap showLn xs
