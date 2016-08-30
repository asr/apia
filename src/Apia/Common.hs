
-- | Common types.

module Apia.Common
  ( ATP(CVC4, E, Equinox, IleanCoP, Metis, OnlineATP, SPASS, Vampire, Z3)
  , Lang(SMT2, TPTP)
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
         | OnlineATP String
         | SPASS
         | Vampire
         | Z3
         deriving Show

instance Pretty ATP where
  pretty (OnlineATP atp) = text . show $ atp
  pretty other           = text . show $ other

-- | Target languages.
data Lang = TPTP   -- ^ FOF (First-order form).
          | SMT2   -- ^ SMT-LIB v2.
          deriving Eq
