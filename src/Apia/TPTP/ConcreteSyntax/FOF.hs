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

import Agda.Syntax.Common
  ( TPTPRole(TPTPAxiom, TPTPConjecture, TPTPDefinition, TPTPHint)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.TPTP.ConcreteSyntax.Common ( G, ToTPTP(toTPTP) )
import Apia.TPTP.Types                 ( AF(AF) )
import Apia.Utils.Text                 ( (+++) )

import Data.Text ( Text )

#include "undefined.h"

------------------------------------------------------------------------------
-- | FOF type synonym.
type FOF = Text

-- | Translation to FOF concrete syntax.
class ToFOF a where
  toFOF ∷ a → G FOF

------------------------------------------------------------------------------

instance ToFOF TPTPRole where
  toFOF TPTPAxiom      = return "axiom"
  toFOF TPTPConjecture = return "conjecture"
  toFOF TPTPDefinition = return "definition"
  toFOF TPTPHint       = return "hypothesis"
  toFOF _              = __IMPOSSIBLE__

-- Translation of annotated formulae to FOF concrete syntax.
instance ToFOF AF where
  toFOF (AF qName atpRole formula) = do
    qName_   ← toTPTP qName
    atpRole_ ← toFOF atpRole
    formula_ ← toTPTP formula

    return $ "fof("
      +++ qName_ +++ ", "
      +++ atpRole_ +++ ", "
      +++ formula_ +++ ")."
      +++ "\n\n"
