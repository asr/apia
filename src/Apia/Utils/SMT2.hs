
-- | Wrapper for the smtLib library.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.Utils.SMT2
  ( smt2And
  , smt2Bicond
  , smt2C
  , smt2Cond
  , smt2False
  , smt2Not
  , smt2Or
  , smt2True
  , SMT2Expr
  , SMT2Type
  , ppText
  , tmpExpr

  -- From the smtLib library
  , Command(CmdAssert, CmdDeclareFun)
  , Script(Script)
  , pp
  , tBool
  ) where

import Apia.Prelude

import Data.Text ( Text )
import qualified Data.Text as T

import SMTLib2      as SMT2
import SMTLib2.Core as SMT2

------------------------------------------------------------------------------

type SMT2Expr = Expr
type SMT2Type = Type

-- TODO (2016-04-30): Remove
tmpExpr ∷ SMT2Expr
tmpExpr = SMT2.or (smt2C "p") (SMT2.not $ smt2C "p")

ppText ∷ PP a ⇒ a → Text
ppText = T.pack . show . pp

smt2False, smt2True ∷ SMT2Expr
smt2False = SMT2.false
smt2True  = SMT2.true

smt2Not ∷ SMT2Expr → SMT2Expr
smt2Not = SMT2.not

smt2And, smt2Cond, smt2Or ∷ SMT2Expr → SMT2Expr → SMT2Expr
smt2And = SMT2.and
smt2Cond = (==>)
smt2Or  = SMT2.or

smt2Bicond ∷ SMT2Expr → SMT2Expr → SMT2Expr
smt2Bicond p q = SMT2.and (p ==> q) (q ==> p)

smt2C ∷ Ident → SMT2Expr
smt2C x = app x []
