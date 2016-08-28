
-- | Target first-order logic types.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.FOL.Types
  ( LFormula( And
            , Bicond
            , Cond
            , Eq
            , Exists
            , FALSE
            , ForAll
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
              | Cond LFormula LFormula
              | Bicond LFormula LFormula
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
  pretty (Cond f1 f2)    = sspaces "Cond" <> pretty f1 <> pretty f2
  pretty (Bicond f1 f2)  = sspaces "Bicond" <> pretty f1 <> pretty f2
  pretty (ForAll var f)  = sspaces "ForAll" <> pretty var <> pretty (f $ Var var)
  pretty (Exists var f)  = sspaces "Exists " <> pretty var <> pretty (f $ Var var)
  pretty (Eq t1 t2)      = sspaces "Eq" <> pretty t1 <> pretty t2
