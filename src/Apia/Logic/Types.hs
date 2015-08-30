------------------------------------------------------------------------------
-- |
-- Module      : Apia.Logic.Types
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Target logic types.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Logic.Types
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
  , LTerm(Fun, Var)
  ) where

import Apia.Utils.PrettyPrint ( (<>), hcat, Pretty(pretty), space, sspaces )

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
data LFormula = TRUE
              | FALSE
              | Predicate String [LTerm]
              | Not LFormula
              | And LFormula LFormula
              | Or LFormula LFormula
              | Implies LFormula LFormula
              | Equiv LFormula LFormula
              | ForAll VarName (LTerm → LFormula)
              | Exists VarName (LTerm → LFormula)

instance Pretty LFormula where
  pretty TRUE                = sspaces "TRUE"
  pretty FALSE               = sspaces "FALSE"
  pretty (Predicate name ts) = sspaces "Predicate" <> pretty name <> space <> pretty ts
  pretty (Not f)             = sspaces "Not" <> pretty f
  pretty (And f1 f2)         = sspaces "And" <> pretty f1 <> pretty f2
  pretty (Or f1 f2)          = sspaces "Or" <> pretty f1 <> pretty f2
  pretty (Implies f1 f2)     = sspaces "Implies" <> pretty f1 <> pretty f2
  pretty (Equiv f1 f2)       = sspaces "Equiv" <> pretty f1 <> pretty f2
  pretty (ForAll var f)      = sspaces "ForAll" <> pretty var <> pretty (f $ Var var)
  pretty (Exists var f)      = sspaces "Exists " <> pretty var <> pretty (f $ Var var)
