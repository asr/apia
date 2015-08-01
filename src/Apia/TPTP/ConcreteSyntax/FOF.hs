------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.ConcreteSyntax.FOF
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

module Apia.TPTP.ConcreteSyntax.FOF
  ( ToFOF(toFOF)
  , FOF  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Apia.TPTP.ConcreteSyntax.Common ( G, ToTPTP(toTPTP) )
import Apia.TPTP.Types                 ( AF(AF) )
import Apia.Utils.Text                 ( (+++) )

import Data.Text ( Text )

------------------------------------------------------------------------------
-- | FOF type synonym.
type FOF = Text

-- | Translation to FOF concrete syntax.
class ToFOF a where
  toFOF ∷ a → G FOF

------------------------------------------------------------------------------
-- Translation of annotated formulae to FOF concrete syntax.

instance ToFOF AF where
  toFOF (AF qName atpRole formula) = do
    qName_   ← toTPTP qName
    atpRole_ ← toTPTP atpRole
    formula_ ← toTPTP formula

    return $ "fof("
      +++ qName_ +++ ", "
      +++ atpRole_ +++ ", "
      +++ formula_ +++ ")."
      +++ "\n\n"
