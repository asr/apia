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

------------------------------------------------------------------------------
-- Adapted from AgdaLight (Plugins.FOL.Types).

-- | Target logic terms.
data LTerm = Fun String [LTerm]
           | Var String
          deriving Show

-- | Target logic formulae.
data LFormula = TRUE
              | FALSE
              | Predicate String [LTerm]
              | Not LFormula
              | And LFormula LFormula
              | Or LFormula LFormula
              | Implies LFormula LFormula
              | Equiv LFormula LFormula
              | ForAll (LTerm → LFormula)
              | Exists (LTerm → LFormula)

instance Show LFormula where
  show TRUE                = " TRUE "
  show FALSE               = " FALSE "
  show (Predicate name ts) = " Predicate " ++ show name ++ " " ++ show ts
  show (Not f)             = " Not " ++ show f
  show (And f1 f2)         = " And " ++ show f1 ++ show f2
  show (Or f1 f2)          = " Or " ++ show f1 ++ show f2
  show (Implies f1 f2)     = " Implies " ++ show f1 ++ show f2
  show (Equiv f1 f2)       = " Equiv " ++ show f1 ++ show f2
  -- show (ForAll var f)      = " ForAll " ++ show var ++ show (f $ Var var)
  -- show (Exists var f)      = " Exists " ++ show var ++ show (f $ Var var)
  show (ForAll _)          = " ForAll " ++ show "TODO"
  show (Exists _)          = " Exists " ++ show "TODO"
