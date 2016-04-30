------------------------------------------------------------------------------
-- |
-- Module      : Apia.FOL.Types
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Target first-order logic types.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.FOL.Types
  ( LFormula( And
            , Eq
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
  , LTerm(Fun, Var)
  , VarName
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Utils.PrettyPrint
  ( (<>)
  , hcat
  , Pretty(pretty)
  , space
  , sspaces
  )

------------------------------------------------------------------------------

type VarName = String

-- Adapted from AgdaLight (Plugins.FOL.Types).

-- | Target logic terms.
data LTerm = Fun String [LTerm]
           | Var VarName

instance Pretty LTerm where
  pretty (Fun f ts) = sspaces "Fun" <> pretty f <> pretty ts
  pretty (Var v)    = sspaces "Var" <> pretty v

instance Pretty [LTerm] where
  pretty = hcat . map pretty

-- | Target logic formulae.

-- | Predicates names.
type PredicateName = String

data LFormula = TRUE
              | FALSE
              | Predicate PredicateName [LTerm]
              | Not LFormula
              | And LFormula LFormula
              | Or LFormula LFormula
              | Implies LFormula LFormula
              | Equiv LFormula LFormula
              | ForAll VarName (LTerm → LFormula)
              | Exists VarName (LTerm → LFormula)
              | Eq LTerm LTerm

instance Pretty LFormula where
  pretty TRUE  = sspaces "TRUE"
  pretty FALSE = sspaces "FALSE"

  pretty (Predicate pName ts) =
    sspaces "Predicate" <> pretty pName <> space <> pretty ts

  pretty (Not f)         = sspaces "Not" <> pretty f
  pretty (And f1 f2)     = sspaces "And" <> pretty f1 <> pretty f2
  pretty (Or f1 f2)      = sspaces "Or" <> pretty f1 <> pretty f2
  pretty (Implies f1 f2) = sspaces "Implies" <> pretty f1 <> pretty f2
  pretty (Equiv f1 f2)   = sspaces "Equiv" <> pretty f1 <> pretty f2
  pretty (ForAll var f)  = sspaces "ForAll" <> pretty var <> pretty (f $ Var var)
  pretty (Exists var f)  = sspaces "Exists " <> pretty var <> pretty (f $ Var var)
  pretty (Eq t1 t2)      = sspaces "Eq" <> pretty t1 <> pretty t2
