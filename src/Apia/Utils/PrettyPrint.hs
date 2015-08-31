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
  , sspaces
  , squotes
  ) where

import Text.PrettyPrint
------------------------------------------------------------------------------
-- Auxiliary functions

-- | Wrap a document in ‘...’.
bquotes ∷ Doc → Doc
bquotes d = char '‘' <> d <> char '’'

-- | Wrap a document in spaces.
spaces ∷ Doc → Doc
spaces d = space <> d <> space

-- | Wrap a string in ‘...’.
squotes ∷ String → Doc
squotes = bquotes . text

-- | Wrap a string in spaces.
sspaces ∷ String → Doc
sspaces = spaces . text

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

instance Pretty a ⇒ Pretty (Maybe a) where
  pretty (Just a) = pretty "Just" <> pretty a
  pretty Nothing  = pretty "Nothing"
