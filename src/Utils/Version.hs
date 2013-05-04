------------------------------------------------------------------------------
-- |
-- Module      : Utils.Version
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Utilities related to 'Version'.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Utils.Version ( progNameVersion )
where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Version ( showVersion )

import System.Environment ( getProgName )

------------------------------------------------------------------------------
-- Local imports

import qualified Paths_apia as P ( version )

import Utils.String ( toUpperFirst )

------------------------------------------------------------------------------

-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
  progName ← fmap toUpperFirst getProgName
  return $ progName ++ " version " ++ showVersion P.version
