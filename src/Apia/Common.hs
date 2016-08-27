
-- | Common types.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Common
  ( ATP(CVC4, E, Equinox, IleanCoP, Metis, SPASS, Vampire, Z3)
  , Lang(SMT2, TPTP)
  , smt2Ext
  , SMT2Role(SMT2Axiom, SMT2Conjecture, SMT2Declaration)
  , tptpExt
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

-- | Target languages.
data Lang = TPTP   -- ^ FOF (First-order form).
          | SMT2   -- ^ SMT-LIB v2.
          deriving (Eq, Show)

-- | SMT-LIB roles.
data SMT2Role = SMT2Axiom
              | SMT2Conjecture
              | SMT2Declaration
              deriving Show

-- | TPTP and SMT-Lib v.2 file extensions.
smt2Ext, tptpExt ∷ String
smt2Ext = ".smt2"
tptpExt = ".fof"
