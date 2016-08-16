
-- | First-order logic constants.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Adapted from AgdaLight (Plugins.FOL.Constants).

module Apia.FOL.Constants
  ( lTrue
  , lFalse
  , lNot
  , lAnd
  , lOr
  , lCond
  , lBicond1
  , lBicond2
  , lForAll
  , lExists
  , lEquals
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

------------------------------------------------------------------------------
-- | Identifiers recognized by the logic translator.
lTrue
  , lFalse
  , lNot
  , lAnd
  , lOr
  , lCond
  , lBicond1
  , lBicond2
  , lExists
  , lForAll
  , lEquals ∷ String
lTrue    = "⊤"
lFalse   = "⊥"
lNot     = "¬"
lAnd     = "∧"
lOr      = "∨"
lCond    = "⇒"  -- The non-dependent function space @→@ can be used
                -- instead.
lBicond1 = "↔"
lBicond2 = "⇔"
lExists  = "∃"
lForAll  = "⋀"  -- The dependent function space @∀ x → A@ can be used
                -- instead.
lEquals  = "≡"
