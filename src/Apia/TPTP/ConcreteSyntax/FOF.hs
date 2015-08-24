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
  )

import Apia.TPTP.ConcreteSyntax.Common
  ( CFP(P)
  , cfpNameToTPTP
  , ToTPTP(toTPTP)
  )

import Apia.TPTP.Types ( AF(AFor) )
import Apia.Utils.Text ( (+++), toUpperFirst )

import Data.Text ( Text )
import qualified Data.Text as T

#include "undefined.h"

------------------------------------------------------------------------------
-- | FOF type synonym.
type FOF = Text

-- | Translation to FOF concrete syntax.
class ToFOF a where
  toFOF ∷ a → FOF

------------------------------------------------------------------------------

instance ToFOF LFormula where
  -- We translate the hard-coded logic predicate @equal_@ as the
  -- predefined equality in the ATP.
  toFOF (Predicate "equal_" [t1, t2] ) =
    "( " +++ toTPTP t1 +++ " = " +++ toTPTP t2 +++ " )"

  toFOF (Predicate "equal_" _) = __IMPOSSIBLE__

  -- If the predicate represents a propositional logic variable,
  -- following the TPTP syntax, we do not print the internal
  -- parenthesis.
  toFOF (Predicate name []) = "( " +++ cfpNameToTPTP P name +++ " )"

  toFOF (Predicate name terms) =
    "( " +++ cfpNameToTPTP P name +++ "(" +++ toTPTP terms +++ ")" +++ " )"

  toFOF (And f1 f2)     = "( " +++ toFOF f1 +++ " & " +++ toFOF f2 +++ " )"
  toFOF (Or f1 f2)      = "( " +++ toFOF f1 +++ " | " +++ toFOF f2 +++ " )"
  toFOF (Not f)         = "( " +++ T.cons '~' (toFOF f) +++ " )"
  toFOF (Implies f1 f2) = "( " +++ toFOF f1 +++ " => " +++ toFOF f2 +++ " )"
  toFOF (Equiv f1 f2)   = "( " +++ toFOF f1 +++ " <=> " +++ toFOF f2 +++ " )"

  toFOF (ForAll var f) =
    "( ! [" +++ toUpperFirst (T.pack var) +++ "] : "
    +++ toFOF (f (Var var))
    +++ " )"

  toFOF (Exists var f) =
    "( ? [" +++ toUpperFirst (T.pack var) +++ "] : "
    +++ toFOF (f (Var var))
    +++ " )"

  toFOF TRUE  = "( " +++ "$true" +++ " )"
  toFOF FALSE = "( " +++ "$false" +++ " )"

instance ToFOF TPTPRole where
  toFOF TPTPAxiom      = "axiom"
  toFOF TPTPConjecture = "conjecture"
  toFOF TPTPDefinition = "definition"
  toFOF TPTPHint       = "hypothesis"
  toFOF _              = __IMPOSSIBLE__

-- Translation of annotated formulae to FOF concrete syntax.
instance ToFOF AF where
  toFOF (AFor qName atpRole formula) =
    "fof("
    +++ toTPTP qName +++ ", "
    +++ toFOF atpRole +++ ", "
    +++ toFOF formula
    +++ ")." +++ "\n\n"

