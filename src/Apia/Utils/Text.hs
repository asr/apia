
-- | Utilities on text.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Utils.Text
  ( (+++)
  , parens
  , toUpperFirst
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Data.Text ( Text )
import qualified Data.Text as T

#include "undefined.h"

------------------------------------------------------------------------------
-- | Append synonymous.
(+++) ∷ Text → Text → Text
(+++) = T.append

-- | Convert the first letter of a text to the corresponding upper-case letter.
toUpperFirst ∷ Text → Text
toUpperFirst xs =
  case T.uncons xs of
    Just (x', xs') → T.cons (toUpper x') xs'
    Nothing        → __IMPOSSIBLE__

-- | Wrap text in ( ... ).
parens ∷ Text → Text
parens t = "(" +++ t +++ ")"
