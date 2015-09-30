{-# LANGUAGE UnicodeSyntax #-}

module Apia.Logic.Translation.ToFormulae.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Common   ( Dom )
import Agda.Syntax.Internal ( Type )

import Apia.Logic.Types ( LFormula )
import Apia.Monad.Base  ( T )

------------------------------------------------------------------------------

agdaDomTypeToFormula ∷ Dom Type → T LFormula
agdaTypeToFormula    ∷ Type → T LFormula
