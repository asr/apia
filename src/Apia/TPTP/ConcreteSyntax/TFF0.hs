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

import Agda.Syntax.Common
  ( TPTPRole(TPTPAxiom, TPTPConjecture, TPTPDefinition, TPTPHint, TPTPType)
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
-- | TFF0 type synonym.
type TFF0 = Text

-- | Translation to TFF0 concrete syntax.
class ToTFF0 a where
  toTFF0 ∷ a → G TFF0

------------------------------------------------------------------------------

instance ToTFF0 LFormula where
  -- We translate the hard-coded logic predicate @equal_@ as the
  -- predefined equality in the ATP.
  toTFF0 (Predicate "equal_" [t1, t2] ) = do
    t1_ ← toTPTP t1
    t2_ ← toTPTP t2
    return $ "( " +++ t1_ +++ " = " +++ t2_ +++ " )"

  toTFF0 (Predicate "equal_" _) = __IMPOSSIBLE__

  -- If the predicate represents a propositional logic variable,
  -- following the TPTP syntax, we do not print the internal
  -- parenthesis.
  toTFF0 (Predicate name []) = do
    name_ ← cfpNameToTPTP P name
    return $ "( " +++ name_ +++ " )"

  toTFF0 (Predicate name terms) = do
    terms_ ← toTPTP terms
    name_ ← cfpNameToTPTP P name
    return $ "( " +++ name_ +++ "(" +++ terms_ +++ ")" +++ " )"

  toTFF0 (And f1 f2) = do
    f1_ ← toTFF0 f1
    f2_ ← toTFF0 f2
    return $ "( " +++ f1_ +++ " & " +++ f2_ +++ " )"

  toTFF0 (Or f1 f2) = do
    f1_ ← toTFF0 f1
    f2_ ← toTFF0 f2
    return $ "( " +++ f1_ +++ " | " +++ f2_ +++ " )"

  toTFF0 (Not f) = do
    f_ ← toTFF0 f
    return $ "( " +++ T.cons '~' f_ +++ " )"

  toTFF0 (Implies f1 f2) = do
    f1_ ← toTFF0 f1
    f2_ ← toTFF0 f2
    return $ "( " +++ f1_ +++ " => " +++ f2_ +++ " )"

  toTFF0 (Equiv f1 f2) = do
    f1_ ← toTFF0 f1
    f2_ ← toTFF0 f2
    return $ "( " +++ f1_ +++ " <=> " +++ f2_ +++ " )"

  toTFF0 (ForAll f) = do
    (freshVar, f_) ← quantifierHelper toTFF0 f

    return $
      "( ! [" +++ toUpperFirst (T.pack freshVar) +++ "] : "
      +++ f_
      +++ " )"

  toTFF0 (Exists f) = do
    (freshVar, f_) ← quantifierHelper toTFF0 f

    return $
      "( ? [" +++ toUpperFirst (T.pack freshVar) +++ "] : "
      +++ f_
      +++ " )"

  toTFF0 TRUE  = return $ "( " +++ "$true" +++ " )"
  toTFF0 FALSE = return $ "( " +++ "$false" +++ " )"

instance ToTFF0 TPTPRole where
  toTFF0 TPTPAxiom      = return "axiom"
  toTFF0 TPTPConjecture = return "conjecture"
  toTFF0 TPTPDefinition = return "definition"
  toTFF0 TPTPHint       = return "hypothesis"
  toTFF0 TPTPType       = return "type"

-- Translation of annotated formulae to TFF0 concrete syntax.
instance ToTFF0 AF where
  toTFF0 (AFor qName atpRole formula) = do
    qName_   ← toTPTP qName
    atpRole_ ← toTFF0 atpRole
    formula_ ← toTFF0 formula

    return $ "tff("
      +++ qName_ +++ ", "
      +++ atpRole_ +++ ", "
      +++ formula_ +++ ")."
      +++ "\n\n"
