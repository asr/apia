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
  )

import Apia.TPTP.ConcreteSyntax.Common
  ( CFP(P)
  , cfpNameToTPTP
  , G
  , quantifierHelper
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
  toFOF ∷ a → G FOF

------------------------------------------------------------------------------

instance ToFOF LFormula where
  -- We translate the hard-coded logic predicate @equal_@ as the
  -- predefined equality in the ATP.
  toFOF (Predicate "equal_" [t1, t2] ) = do
    t1_ ← toTPTP t1
    t2_ ← toTPTP t2
    return $ "( " +++ t1_ +++ " = " +++ t2_ +++ " )"

  toFOF (Predicate "equal_" _) = __IMPOSSIBLE__

  -- If the predicate represents a propositional logic variable,
  -- following the TPTP syntax, we do not print the internal
  -- parenthesis.
  toFOF (Predicate name []) = do
    name_ ← cfpNameToTPTP P name
    return $ "( " +++ name_ +++ " )"

  toFOF (Predicate name terms) = do
    terms_ ← toTPTP terms
    name_ ← cfpNameToTPTP P name
    return $ "( " +++ name_ +++ "(" +++ terms_ +++ ")" +++ " )"

  toFOF (And f1 f2) = do
    f1_ ← toFOF f1
    f2_ ← toFOF f2
    return $ "( " +++ f1_ +++ " & " +++ f2_ +++ " )"

  toFOF (Or f1 f2) = do
    f1_ ← toFOF f1
    f2_ ← toFOF f2
    return $ "( " +++ f1_ +++ " | " +++ f2_ +++ " )"

  toFOF (Not f) = do
    f_ ← toFOF f
    return $ "( " +++ T.cons '~' f_ +++ " )"

  toFOF (Implies f1 f2) = do
    f1_ ← toFOF f1
    f2_ ← toFOF f2
    return $ "( " +++ f1_ +++ " => " +++ f2_ +++ " )"

  toFOF (Equiv f1 f2) = do
    f1_ ← toFOF f1
    f2_ ← toFOF f2
    return $ "( " +++ f1_ +++ " <=> " +++ f2_ +++ " )"

  toFOF (ForAll f) = do
    (freshVar, f_) ← quantifierHelper toFOF f

    return $
      "( ! [" +++ toUpperFirst (T.pack freshVar) +++ "] : "
      +++ f_
      +++ " )"

  toFOF (Exists f) = do
    (freshVar, f_) ← quantifierHelper toFOF f

    return $
      "( ? [" +++ toUpperFirst (T.pack freshVar) +++ "] : "
      +++ f_
      +++ " )"

  toFOF TRUE  = return $ "( " +++ "$true" +++ " )"
  toFOF FALSE = return $ "( " +++ "$false" +++ " )"

instance ToFOF TPTPRole where
  toFOF TPTPAxiom      = return "axiom"
  toFOF TPTPConjecture = return "conjecture"
  toFOF TPTPDefinition = return "definition"
  toFOF TPTPHint       = return "hypothesis"
  toFOF _              = __IMPOSSIBLE__

-- Translation of annotated formulae to FOF concrete syntax.
instance ToFOF AF where
  toFOF (AFor qName atpRole formula) = do
    qName_   ← toTPTP qName
    atpRole_ ← toFOF atpRole
    formula_ ← toFOF formula

    return $ "fof("
      +++ qName_ +++ ", "
      +++ atpRole_ +++ ", "
      +++ formula_ +++ ")."
      +++ "\n\n"
