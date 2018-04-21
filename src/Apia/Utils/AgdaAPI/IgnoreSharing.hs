
-- | Ignoring sharing.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) ) where

------------------------------------------------------------------------------

import Apia.Prelude hiding ( sort )

import Agda.Syntax.Common
  ( Arg(Arg)
  , Dom(Dom)
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Elim'(Apply, IApply, Proj)
  , Tele(EmptyTel, ExtendTel)
  , Term(Def, Pi)
  , Type'(El)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
-- import Agda.Utils.Pointer ( derefPtr )

#include "undefined.h"

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

  -- ignoreSharing (Shared ptr) = ignoreSharing $ derefPtr ptr

  ignoreSharing term = term

instance IgnoreSharing a ⇒ IgnoreSharing (Dom a) where
  ignoreSharing (Dom ai b e) = Dom ai b $ ignoreSharing e

instance IgnoreSharing a ⇒ IgnoreSharing (Elim' a) where
  ignoreSharing (Apply a) = Apply $ ignoreSharing a
  ignoreSharing p@Proj{}  = p
  ignoreSharing IApply{}  = __IMPOSSIBLE__

instance IgnoreSharing a ⇒ IgnoreSharing (Arg a) where
  ignoreSharing (Arg ai e) = Arg ai (ignoreSharing e)

instance IgnoreSharing a ⇒ IgnoreSharing (Tele a) where
  ignoreSharing EmptyTel        = EmptyTel
  ignoreSharing (ExtendTel a b) = ExtendTel (ignoreSharing a) (ignoreSharing b)

instance IgnoreSharing a ⇒ IgnoreSharing (Abs a) where
  ignoreSharing (Abs name a)   = Abs name $ ignoreSharing a
  ignoreSharing (NoAbs name a) = NoAbs name $ ignoreSharing a

instance IgnoreSharing a ⇒ IgnoreSharing [a] where
  ignoreSharing = map ignoreSharing
