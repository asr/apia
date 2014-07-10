{-# LANGUAGE UnicodeSyntax #-}

module Apia.FOL.Translation.Types
  ( domTypeToFormula
  , typeToFormula
  ) where

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Internal as I ( Dom, Type )

------------------------------------------------------------------------------
-- Apia imports

import Apia.FOL.Types  ( FOLFormula )
import Apia.Monad.Base ( T )

------------------------------------------------------------------------------

domTypeToFormula ∷ I.Dom Type → T FOLFormula
typeToFormula    ∷ Type → T FOLFormula
