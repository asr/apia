{-# LANGUAGE UnicodeSyntax #-}

module Apia.FOL.Translation.Types
  ( domTypeToFormula
  , typeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Internal as I ( Dom, Type )

import Apia.FOL.Types  ( FOLFormula )
import Apia.Monad.Base ( T )

------------------------------------------------------------------------------

domTypeToFormula ∷ I.Dom Type → T FOLFormula
typeToFormula    ∷ Type → T FOLFormula
