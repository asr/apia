------------------------------------------------------------------------------
-- |
-- Module      : TPTP.ConcreteSyntax.TFF0
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- TPTP TFFO (typed first-order form, without arithmetic) concrete
-- syntax
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TPTP.ConcreteSyntax.TFF0
  ( ToTFF0(toTFF0)
  , TFF0  -- Required by Haddock.
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
-- | TFF0 type synonym.
type TFF0 = Text

-- | Translation to TFF0 concrete syntax.
class ToTFF0 a where
  toTFF0 ∷ a → TFF0

------------------------------------------------------------------------------
-- Translation of annotated formulae to TFF0 concrete syntax.

instance ToTFF0 AF where
  toTFF0 (AF qName atpRole formula) =
    "tff("
    +++ toTPTP qName +++ ", "
    +++ toTPTP atpRole +++ ", "
    +++ toTPTP formula
    +++ ")." +++ "\n\n"