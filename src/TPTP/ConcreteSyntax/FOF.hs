------------------------------------------------------------------------------
-- |
-- Module      : TPTP.ConcreteSyntax.FOF
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- TPTP FOF (first-order form) concrete syntax
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TPTP.ConcreteSyntax.FOF
  ( ToFOF(toFOF)
  , FOF  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Text ( Text )

------------------------------------------------------------------------------
-- Apia imports

import TPTP.ConcreteSyntax.Common ( ToTPTP(toTPTP) )
import TPTP.Types                 ( AF(AF) )
import Utils.Text                 ( (+++) )

------------------------------------------------------------------------------
-- | FOF type synonym.
type FOF = Text

-- | Translation to FOF concrete syntax.
class ToFOF a where
  toFOF ∷ a → FOF

------------------------------------------------------------------------------
-- Translation of annotated formulae to FOF concrete syntax.

instance ToFOF AF where
  toFOF (AF qName atpRole formula) =
    "fof("
    +++ toTPTP qName +++ ", "
    +++ toTPTP atpRole +++ ", "
    +++ toTPTP formula
    +++ ")." +++ "\n\n"
