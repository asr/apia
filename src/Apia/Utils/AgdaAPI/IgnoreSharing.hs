------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.AgdaAPI.IgnoreSharing
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Ignoring sharing
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) ) where

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Common
  ( Arg(Arg)
  , ArgInfo(ArgInfo)
  , Dom(Dom)
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Elim'(Apply, Proj)
  , Tele(EmptyTel, ExtendTel)
  , Term(Def, Shared, Pi)
  , Type'(El)
  )

import Agda.Utils.Pointer ( derefPtr )

------------------------------------------------------------------------------

-- | Ignoring sharing.
class IgnoreSharing a where
  ignoreSharing ∷ a → a

instance IgnoreSharing a ⇒ IgnoreSharing (Type' a) where
  ignoreSharing (El sort a) = El sort $ ignoreSharing a

instance IgnoreSharing Term where
  ignoreSharing (Def qname elims) = Def qname $ ignoreSharing elims

  ignoreSharing (Pi domTy (Abs x absTy)) =
    Pi (ignoreSharing domTy) (Abs x (ignoreSharing absTy))

  ignoreSharing (Pi domTy (NoAbs x absTy)) =
    Pi (ignoreSharing domTy) (NoAbs x (ignoreSharing absTy))

  ignoreSharing (Shared ptr) = ignoreSharing $ derefPtr ptr

  ignoreSharing term = term

instance (IgnoreSharing c, IgnoreSharing e) ⇒ IgnoreSharing (Dom c e) where
  ignoreSharing (Dom c e) = Dom (ignoreSharing c) (ignoreSharing e)

instance IgnoreSharing c ⇒ IgnoreSharing (ArgInfo c) where
  ignoreSharing (ArgInfo h r xs) = ArgInfo h r (ignoreSharing xs)

instance IgnoreSharing a ⇒ IgnoreSharing (Elim' a) where
  ignoreSharing (Apply a)    = Apply $ ignoreSharing a
  ignoreSharing (Proj qname) = Proj qname

instance (IgnoreSharing c, IgnoreSharing e) ⇒ IgnoreSharing (Arg c e) where
  ignoreSharing (Arg c e) = Arg (ignoreSharing c) (ignoreSharing e)

instance IgnoreSharing a ⇒ IgnoreSharing (Tele a) where
  ignoreSharing EmptyTel        = EmptyTel
  ignoreSharing (ExtendTel a b) = ExtendTel (ignoreSharing a) (ignoreSharing b)

instance IgnoreSharing a ⇒ IgnoreSharing (Abs a) where
  ignoreSharing (Abs name a)   = Abs name $ ignoreSharing a
  ignoreSharing (NoAbs name a) = NoAbs name $ ignoreSharing a

instance IgnoreSharing a ⇒ IgnoreSharing [a] where
  ignoreSharing = map ignoreSharing
