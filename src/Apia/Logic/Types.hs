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
  , LType(LType)
  , VarName
  ) where

import Apia.Utils.PrettyPrint
  ( (<>)
  , Doc
  , hcat
  , Pretty(pretty)
  , space
  , sspaces
  )

------------------------------------------------------------------------------

-- | Target logic types.
newtype LType = LType String

instance Pretty (Maybe LType) where
  pretty (Just (LType ty)) = pretty ty
  pretty Nothing           = pretty "Nothing"

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

data LFormula = TRUE
              | FALSE
              | Predicate String [LTerm]
              | Not LFormula
              | And LFormula LFormula
              | Or LFormula LFormula
              | Implies LFormula LFormula
              | Equiv LFormula LFormula
              | ForAll VarName (Maybe LType) (LTerm → LFormula)
              | Exists VarName (Maybe LType) (LTerm → LFormula)

prettyQuantifierBody ∷ VarName → Maybe LType → (LTerm → LFormula) → Doc
prettyQuantifierBody var ty f =
  pretty var <> sspaces ":" <> pretty ty <> pretty (f $ Var var)

instance Pretty LFormula where
  pretty TRUE                = sspaces "TRUE"
  pretty FALSE               = sspaces "FALSE"
  pretty (Predicate name ts) = sspaces "Predicate" <> pretty name <> space <> pretty ts
  pretty (Not f)             = sspaces "Not" <> pretty f
  pretty (And f1 f2)         = sspaces "And" <> pretty f1 <> pretty f2
  pretty (Or f1 f2)          = sspaces "Or" <> pretty f1 <> pretty f2
  pretty (Implies f1 f2)     = sspaces "Implies" <> pretty f1 <> pretty f2
  pretty (Equiv f1 f2)       = sspaces "Equiv" <> pretty f1 <> pretty f2
  pretty (ForAll var ty f)   = sspaces "ForAll" <> prettyQuantifierBody var ty f
  pretty (Exists var ty f)   = sspaces "Exists" <> prettyQuantifierBody var ty f
