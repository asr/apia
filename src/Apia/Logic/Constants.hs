-----------------------------------------------------------------------------
-- |
-- Module      : Apia.Logic.Constants
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- The logic constants.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Adapted from AgdaLight (Plugins.FOL.Constants).

module Apia.Logic.Constants
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