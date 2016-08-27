
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Translation.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Common   ( Dom )
import Agda.Syntax.Internal ( Type )

import Apia.Monad.Base       ( T )
import Apia.TargetLang.Types ( TargetFormula )

------------------------------------------------------------------------------

agdaDomTypeToFormula ∷ Dom Type → T TargetFormula
agdaTypeToFormula    ∷ Type → T TargetFormula
