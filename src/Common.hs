-----------------------------------------------------------------------------
-- |
-- Module      : Common
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Common types
-----------------------------------------------------------------------------

module Common
  ( ATP(CVC4, E, Equinox, IleanCoP, Metis, SPASS, Vampire, Z3))
  where

------------------------------------------------------------------------------
-- | The ATPs (first-order ATPs and SMT solvers).
data ATP = CVC4
         | E
         | Equinox
         | IleanCoP
         | Metis
         | SPASS
         | Vampire
         | Z3
           deriving Show
