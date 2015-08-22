------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.PrettyPrint
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities for pretty printing.
------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Utils.PrettyPrint
  ( module Text.PrettyPrint
  , Pretty(pretty)
  , prettyShow
  , squotes
  ) where

import Text.PrettyPrint
------------------------------------------------------------------------------
-- Auxiliary functions

-- | Wrap document in ‘...’.
bquotes ∷ Doc → Doc
bquotes p = char '‘' <> p <> char '’'

-- | Wrap a string in ‘...’.
squotes ∷ String → Doc
squotes = bquotes . text

-- | Use instead of 'show' when printing to world.
prettyShow :: Pretty a ⇒ a → String
prettyShow = render . pretty

------------------------------------------------------------------------------

-- | Pretty print type class.
class Pretty a where
  pretty ∷ a → Doc

instance Pretty Doc where
  pretty = id

instance Pretty String where
  pretty = text
