------------------------------------------------------------------------------
-- |
-- Module      : Apia.TPTP.ConcreteSyntax.TFF0
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

module Apia.TPTP.ConcreteSyntax.TFF0
  ( ToTFF0(toTFF0)
  , TFF0  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Apia.TPTP.ConcreteSyntax.Common ( G, ToTPTP(toTPTP) )
import Apia.TPTP.Types                 ( AF(AF) )
import Apia.Utils.Text                 ( (+++) )

import Data.Text ( Text )

------------------------------------------------------------------------------
-- | TFF0 type synonym.
type TFF0 = Text

-- | Translation to TFF0 concrete syntax.
class ToTFF0 a where
  toTFF0 ∷ a → G TFF0

------------------------------------------------------------------------------
-- Translation of annotated formulae to TFF0 concrete syntax.

instance ToTFF0 AF where
  toTFF0 (AF qName atpRole formula) = do
    qName_   ← toTPTP qName
    atpRole_ ← toTPTP atpRole
    formula_ ← toTPTP formula

    return $ "tff("
      +++ qName_ +++ ", "
      +++ atpRole_ +++ ", "
      +++ formula_ +++ ")."
      +++ "\n\n"

