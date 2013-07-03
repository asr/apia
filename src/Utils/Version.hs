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

{-# LANGUAGE UnicodeSyntax #-}

module Utils.Version ( progNameVersion )
where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Version ( showVersion )

import Distribution.Package                  ( PackageIdentifier(pkgVersion) )
import Distribution.PackageDescription       ( package , packageDescription )
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Verbosity                ( silent )

import System.Environment ( getProgName )

------------------------------------------------------------------------------
-- Local imports

-- import qualified Paths_apia as P ( version )

import Utils.String ( toUpperFirst )

------------------------------------------------------------------------------
-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
  progName ← getProgName
  version  ← fmap (showVersion . pkgVersion . package . packageDescription)
                  $ readPackageDescription silent (progName ++ ".cabal")
  return $ toUpperFirst progName ++ " version " ++ version
