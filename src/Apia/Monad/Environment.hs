
-- | Functions for initializing the translation monad environment.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Environment ( env ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Defaults    ( defaultOptions, getDefaults )
import Apia.Options     ( Options, optNoConfigFile, processOptions )
import Apia.Utils.Monad ( die )

import System.Environment ( getArgs )

------------------------------------------------------------------------------
-- | The environment.
env ∷ IO Options
env = do
  args ← getArgs
  defaults ← getDefaults
  case processOptions args defaults of
    Left err   → die err
    Right opts →
      if optNoConfigFile opts
      then case processOptions args defaultOptions of
        Left err → die err
        Right o → return o
      else return opts
