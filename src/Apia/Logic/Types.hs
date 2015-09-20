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
  , LType(AType, BType)
  , TypeName
  , VarName
  ) where

import Agda.Syntax.Abstract.Name ( QName )

import Apia.Utils.PrettyPrint
  ( (<>)
  , Doc
  , hcat
  , Pretty(pretty)
  , space
  , sspaces
  )

------------------------------------------------------------------------------

-- | Types names.
type TypeName = String

-- | Target logic types.
data LType = BType TypeName        -- ^ Base type.
           | AType TypeName QName  -- ^ Atomic type.

instance Pretty LType where
  pretty (BType tyName)   = pretty tyName
  pretty (AType tyName _) = pretty tyName

-- | Variables names.
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

-- When @Maybe LType@ is equal to @Nothing@, we have mono-sorted
-- first-order logic, otherwise, we have many-sorted first-order
-- logic.

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
              | ForAll VarName (Maybe LType) (LTerm → LFormula)
              | Exists VarName (Maybe LType) (LTerm → LFormula)
              | Eq LTerm LTerm

prettyQuantifierBody ∷ VarName → Maybe LType → (LTerm → LFormula) → Doc
prettyQuantifierBody vName ty f =
  pretty vName <> sspaces ":" <> pretty ty <> pretty (f $ Var vName)

instance Pretty LFormula where
  pretty TRUE  = sspaces "TRUE"
  pretty FALSE = sspaces "FALSE"

  pretty (Predicate pName ts) =
    sspaces "Predicate" <> pretty pName <> space <> pretty ts

  pretty (Not f)             = sspaces "Not" <> pretty f
  pretty (And f1 f2)         = sspaces "And" <> pretty f1 <> pretty f2
  pretty (Or f1 f2)          = sspaces "Or" <> pretty f1 <> pretty f2
  pretty (Implies f1 f2)     = sspaces "Implies" <> pretty f1 <> pretty f2
  pretty (Equiv f1 f2)       = sspaces "Equiv" <> pretty f1 <> pretty f2
  pretty (ForAll vName ty f) = sspaces "ForAll" <> prettyQuantifierBody vName ty f
  pretty (Exists vName ty f) = sspaces "Exists" <> prettyQuantifierBody vName ty f
  pretty (Eq t1 t2)          = sspaces "Eq" <> pretty t1 <> pretty t2
