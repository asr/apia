-----------------------------------------------------------------------------
-- |
-- Module      : Apia.Common
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Common types
-----------------------------------------------------------------------------

module Apia.Common
  ( ATP(CVC4, E, Equinox, IleanCoP, Metis, SPASS, Vampire, Z3)
  , Lang(FOF)
  )
  where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Utils.PrettyPrint ( Pretty(pretty), text )

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

instance Pretty ATP where
  pretty = text . show

-- | TPTP output languages.
data Lang = FOF   -- ^ First-order form.

