{-# LANGUAGE UnicodeSyntax #-}

module Apia.Logic.Translation.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Internal as I ( Dom, Type )

import Apia.Logic.Types ( LFormula )
import Apia.Monad.Base  ( T )

------------------------------------------------------------------------------

agdaDomTypeToFormula ∷ I.Dom Type → T LFormula
agdaTypeToFormula    ∷ Type → T LFormula
