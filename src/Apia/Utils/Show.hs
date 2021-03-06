
-- | Utilities related to 'Show'.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Show
  ( showListLn
  , showLn
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

------------------------------------------------------------------------------
-- | Version of 'show' adding a newline character.
showLn ∷ Show a ⇒ a → String
showLn = (++ "\n") . show

-- | Version of 'show' on lists where the elements are separated by
-- newline characters.
showListLn ∷ Show a ⇒ [a] → String
showListLn [] = "[]"
showListLn xs = concatMap showLn xs
