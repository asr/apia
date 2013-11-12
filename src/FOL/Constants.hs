-----------------------------------------------------------------------------
-- |
-- Module      : FOL.Constants
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- The first-order logic constants.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Adapted from AgdaLight (Plugins.FOL.Constants).

module FOL.Constants
  ( folTrue
  , folFalse
  , folNot
  , folAnd
  , folOr
  , folCond
  , folBicond1
  , folBicond2
  , folForAll
  , folExists
  , folEquals
  ) where

------------------------------------------------------------------------------
-- | Identifiers recognized by the first-order logic translator.
folTrue
  , folFalse
  , folNot
  , folAnd
  , folOr
  , folCond
  , folBicond1
  , folBicond2
  , folExists
  , folForAll
  , folEquals ∷ String
folTrue    = "⊤"
folFalse   = "⊥"
folNot     = "¬"
folAnd     = "∧"
folOr      = "∨"
folCond    = "⇒"  -- The non-dependent function space @→@ can be used
                  -- instead.
folBicond1 = "↔"
folBicond2 = "⇔"
folExists  = "∃"
folForAll  = "⋀"  -- The dependent function space @∀ x → A@ can be
                  -- used instead.
folEquals  = "≡"
