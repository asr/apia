
-- | Utilities related to representation of versions.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Version ( progNameVersion ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Utils.CommitVersion ( getVersion )
import Apia.Utils.String        ( toUpperFirst )

import Data.Version       ( showVersion )
import System.Environment ( getProgName )

import Paths_apia ( version )

------------------------------------------------------------------------------
-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
  commitVersion ← getVersion version
  progName ← getProgName
  return $ toUpperFirst progName ++ " version " ++ showVersion commitVersion
