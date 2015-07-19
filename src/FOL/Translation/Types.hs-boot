{-# LANGUAGE UnicodeSyntax #-}

module FOL.Translation.Types
  ( domTypeToFormula
  , typeToFormula
  ) where

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Internal as I ( Dom, Type )

------------------------------------------------------------------------------
-- Apia imports

import FOL.Types  ( FOLFormula )
import Monad.Base ( T )

------------------------------------------------------------------------------

domTypeToFormula ∷ I.Dom Type → T FOLFormula
typeToFormula    ∷ Type → T FOLFormula
