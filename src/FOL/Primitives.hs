------------------------------------------------------------------------------
-- |
-- Module      : FOL.Primitives
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Names hard-coded in the translation of Agda internal types to
-- first-order logic formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Adapted from AgdaLight (Plugins.FOL.Primitive).

module FOL.Primitives ( app, equal, predicateTranslation )
where

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Impossible ( Impossible(Impossible) , throwImpossible )

------------------------------------------------------------------------------
-- Local imports

import FOL.Types ( FOLTerm(FOLFun), FOLFormula(Predicate) )

#include "../undefined.h"

------------------------------------------------------------------------------

kApp ∷ String
kApp = "app_"

-- | Translation of first-order logic functions using the command-line
-- option @--with-function-application@. For example, the function
-- @foo x1 ... xn@ will be translated to @app_(... app_(app_(foo, x1),
-- x2), ..., xn)@, where @app_@ is a hard-coded binary function
-- symbol.
app ∷ FOLTerm → FOLTerm → FOLTerm
app t1 t2 = FOLFun kApp [t1, t2]

-- | Translation of first-order logic predicates by default. For
-- example, the predicate @P x1 x2 x3@ will be translated to @kp3_(p,
-- x1, x2, x3)@, where @kp3_@ is a hard-coded 4-ary predicate
-- symbol. Using the option @--without-predicate-symbols@ the
-- predicates are translated directly.
predicateTranslation ∷ FOLTerm → [FOLTerm] → FOLFormula
predicateTranslation _ [] = __IMPOSSIBLE__
predicateTranslation p ts = Predicate name (p : ts)
  where name ∷ String
        name = "kp" ++ show (length ts) ++ "_"

-- The constant @kEqual@ refers to the predefined equality in the
-- ATPs.
--
-- N.B. The value of @kEqual@ is ***hard-coded*** in the module
-- TPTP.ConcreteSyntax.
kEqual ∷ String
kEqual = "equal_"

-- | Translation to first-order logic equality.
equal ∷ FOLTerm → FOLTerm → FOLFormula
equal t1 t2 = Predicate kEqual [t1, t2]
