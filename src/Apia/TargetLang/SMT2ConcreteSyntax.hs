------------------------------------------------------------------------------
-- |
-- Module      : Apia.TargetLang.SMT2ConcreteSyntax
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- SMT-LIB v2 concrete syntax.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.TargetLang.SMT2ConcreteSyntax
  ( ToSMT2(toSMT2)
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Common
  ( SMT2Role(SMT2Conjecture, SMT2Declaration)
  )

import Apia.TargetLang.Types ( AF(AExpr, ATy) )

import Apia.Utils.SMT2
  ( Command(CmdAssert, CmdDeclareFun)
  , smt2Not
  , ppText
  , Script(Script)
  )

import Data.Text ( Text )

#include "undefined.h"

------------------------------------------------------------------------------

-- | SMT2 type synonym.
type SMT2 = Text

-- | Translation to SMT-LIB v2 concrete syntax.
class ToSMT2 a where
  toSMT2 ∷ a → SMT2

------------------------------------------------------------------------------

-- Translation of annotated formulae to SMT-LIB v2 concrete syntax.
instance ToSMT2 AF where
  toSMT2 (AExpr _ SMT2Conjecture expr) =
    ppText $ Script [ CmdAssert $ smt2Not expr ]

  toSMT2 (ATy name SMT2Declaration ty) =
    ppText $ Script [ CmdDeclareFun (fromString name) [] ty ]

  toSMT2 _ = __IMPOSSIBLE__
