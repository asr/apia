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

import Agda.Syntax.Common ( TPTPRole(TPTPType) )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Logic.Types
  ( LFormula( And
            , Equiv
            , Exists
            , FALSE
            , ForAll
            , Implies
            , Not
            , Or
            , Predicate
            , TRUE
           )
  , LTerm(Var)
  , VarName
  )

import Apia.TPTP.ConcreteSyntax.Common
  ( CFP(P)
  , cfpNameToTPTP
  , ToTPTP(toTPTP)
  )

import Apia.TPTP.Types ( AF(AFor) )
import Apia.Utils.Text ( (+++), parens, toUpperFirst )

import Data.Text ( Text )
import qualified Data.Text as T

#include "undefined.h"

------------------------------------------------------------------------------
-- | TFF0 type synonym.
type TFF0 = Text

-- | Translation to TFF0 concrete syntax.
class ToTFF0 a where
  toTFF0 ∷ a → TFF0

------------------------------------------------------------------------------

quantifierBodyToTFF0 ∷ VarName → (LTerm → LFormula) → TFF0
quantifierBodyToTFF0 var f =
  "[" +++ toUpperFirst (T.pack var) +++ "] : "
  +++ toTFF0 (f (Var var))

instance ToTFF0 LFormula where
  -- We translate the hard-coded logic predicate @equal_@ as the
  -- predefined equality in the ATP.
  toTFF0 (Predicate "equal_" [t1, t2] ) =
    parens $ toTPTP t1 +++ " = " +++ toTPTP t2

  toTFF0 (Predicate "equal_" _) = __IMPOSSIBLE__

  -- If the predicate represents a propositional logic variable,
  -- following the TPTP syntax, we do not print the internal
  -- parenthesis.
  toTFF0 (Predicate name []) = parens $ cfpNameToTPTP P name

  toTFF0 (Predicate name terms) =
    cfpNameToTPTP P name +++ parens (toTPTP terms)

  toTFF0 (And f1 f2)     = parens $ toTFF0 f1 +++ " & " +++ toTFF0 f2
  toTFF0 (Or f1 f2)      = parens $ toTFF0 f1 +++ " | " +++ toTFF0 f2
  toTFF0 (Not f)         = parens $ T.cons '~' (toTFF0 f)
  toTFF0 (Implies f1 f2) = parens $ toTFF0 f1 +++ " => " +++ toTFF0 f2
  toTFF0 (Equiv f1 f2)   = parens $ toTFF0 f1 +++ " <=> " +++ toTFF0 f2
  toTFF0 (ForAll var f)  = "( ! " +++ quantifierBodyToTFF0 var f +++ " )"
  toTFF0 (Exists var f)  = "( ? " +++ quantifierBodyToTFF0 var f +++ " )"
  toTFF0 TRUE            = parens "$true"
  toTFF0 FALSE           = parens "$false"

instance ToTFF0 TPTPRole where
  toTFF0 TPTPType = "type"
  toTFF0 r        = toTPTP r

-- Translation of annotated formulae to TFF0 concrete syntax.
instance ToTFF0 AF where
  toTFF0 (AFor qName atpRole formula) =
    "tff("
    +++ toTPTP qName +++ ", "
    +++ toTFF0 atpRole +++ ", "
    +++ toTFF0 formula
    +++ ")." +++ "\n\n"
