------------------------------------------------------------------------------
-- |
-- Module      : Utils.Version
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities related to representation of versions.
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Utils.Version ( progNameVersion ) where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Version       ( showVersion )
import System.Environment ( getProgName )

------------------------------------------------------------------------------
-- Apia imports

import Utils.String ( toUpperFirst )
import Paths_apia   ( version )

------------------------------------------------------------------------------
-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
  progName ← getProgName
  return $ toUpperFirst progName ++ " version " ++ showVersion version
