
-- | Utilities for pretty printing.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Utils.PrettyPrint
  ( module Text.PrettyPrint
  , bquotes
  , Pretty(pretty)
  , prettyShow
  , spaces
  , sspaces
  , squotes
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Utils.SMT2 ( pp, SMT2Expr, SMT2Type )
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

instance Pretty SMT2Expr where
  pretty = pp

instance Pretty SMT2Type where
  pretty = pp
