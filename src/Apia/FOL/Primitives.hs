------------------------------------------------------------------------------
-- |
-- Module      : Apia.FOL.Primitives
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Names hard-coded in the translation of Agda internal types to
-- first-order logic formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Adapted from AgdaLight (Plugins.FOL.Primitive).

module Apia.FOL.Primitives
  ( appF
  , appP
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Impossible ( Impossible(Impossible) , throwImpossible )

import Apia.FOL.Types ( LFormula(Predicate), LTerm(Fun) )

#include "undefined.h"

------------------------------------------------------------------------------

kApp ∷ String
kApp = "app_"

-- | Translation of first-order logic functions using the command-line
-- option @--with-function-application@.
-- For example, the function @foo x1 ... xn@ will be translated to
-- @app(... app(app(foo,x1),x2), ...,xn)@, where @app@ is a hard-coded
-- binary function symbol.
appF ∷ LTerm → LTerm → LTerm
appF t1 t2 = Fun kApp [t1, t2]

-- | Translation of first-order logic predicates by default.
-- For example, the predicate @P x1 x2 x3@ will be translated to
-- @kp3(p,x1,x2,x3)@, where @kp3@ is a hard-coded 4-ary predicate
-- symbol. Using the option @--without-predicate-symbols@ the
-- predicates are translated directly.
appP ∷ LTerm → [LTerm] → LFormula
appP _ [] = __IMPOSSIBLE__
appP p ts = Predicate pName (p : ts)
  where pName ∷ String
        pName = "kp" ++ show (length ts) ++ "_"
