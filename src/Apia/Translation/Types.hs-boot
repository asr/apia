
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Translation.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Common   ( Dom )
import Agda.Syntax.Internal ( Type )

import Apia.FOL.Types  ( LFormula )
import Apia.Monad.Base ( T )

------------------------------------------------------------------------------

agdaDomTypeToFormula ∷ Dom Type → T LFormula
agdaTypeToFormula    ∷ Type → T LFormula
