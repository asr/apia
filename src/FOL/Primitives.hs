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

module FOL.Primitives ( appF, appP, equal )
where

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Impossible ( Impossible(Impossible) , throwImpossible )

------------------------------------------------------------------------------
-- Local imports

import FOL.Types ( FOLTerm(FOLFun), FOLFormula(Predicate) )

#include "../undefined.h"

------------------------------------------------------------------------------

kAppF ∷ String
kAppF = "kAppF"

-- | Translation to first-order logic functions. For example, the
-- function @foo x1 ... xn@ will be translate to @kAppF (... kAppF
-- (kAppF(foo, x1), x2), ..., xn)@, where @kAppF@ is a hard-coded
-- binary function symbol.
appF ∷ FOLTerm → FOLTerm → FOLTerm
appF t1 t2 = FOLFun kAppF [t1, t2]

-- | Translation to first-order logic predicates. For example, the
-- predicate @P x1 x2 x3@ will be translate to @kAppP3 (p, x1, x2,
-- x3)@, where @kAppP3@ is a hard-coded constant 4-ary predicate
-- symbol.
appP ∷ FOLTerm → [FOLTerm] → FOLFormula
appP _ [] = __IMPOSSIBLE__
appP p ts = Predicate name (p : ts)
  where name ∷ String
        name = "kAppP" ++ show (length ts)

-- The constant @kEqual@ refers to the predefined equality in the
-- ATPs.
-- N.B. The name @kEqual@ is ***hard-coded*** in the module
-- TPTP.ConcreteSyntax.
kEqual ∷ String
kEqual = "kEqual"

-- | Translation to first-order logic equality.
equal ∷ FOLTerm → FOLTerm → FOLFormula
equal t1 t2 = Predicate kEqual [t1, t2]
