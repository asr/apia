------------------------------------------------------------------------------
-- |
-- Module      : Apia.Monad.Environment
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Functions for initializing the translation monad environment.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Environment ( env ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Defaults    ( getDefaults )
import Apia.Options     ( Options, processOptions )
import Apia.Utils.Monad ( die )

import System.Environment ( getArgs )

------------------------------------------------------------------------------
-- | The environment.
env ∷ IO Options
env = do
  args ← getArgs
  defaults ← getDefaults
  case processOptions args defaults of
    Left err → die err
    Right o  → return o
