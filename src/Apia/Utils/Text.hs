------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.Text
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities on text.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Text ( (+++), toUpperFirst ) where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Char ( toUpper )

import Data.Text ( Text )
import qualified Data.Text as T

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

------------------------------------------------------------------------------
-- Apia imports

#include "../undefined.h"

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
