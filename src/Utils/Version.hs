------------------------------------------------------------------------------
-- |
-- Module      : Utils.Version
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Utilities related to representation versions.
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Utils.Version ( progNameVersion )
where

------------------------------------------------------------------------------
-- Haskell imports

import Data.List ( intercalate )

import Distribution.Package                  ( PackageIdentifier(pkgVersion) )
import Distribution.PackageDescription       ( package , packageDescription )
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Verbosity                ( silent )
import Distribution.Version                  ( Version(Version) )

import System.Environment ( getProgName )

------------------------------------------------------------------------------
-- Local imports

-- import qualified Paths_apia as P ( version )

import Utils.String ( toUpperFirst )

------------------------------------------------------------------------------

showVersion ∷ Version → String
showVersion (Version branch _) = intercalate "." (map show branch)

-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
  progName ← getProgName

  -- 03 July 2013. We don't use the generated module from Cabal
  -- (Paths_pkgname) for getting the version because this module
  -- doesn't pass the -fwarn-missing-import-lists warning.
  version  ← fmap (showVersion . pkgVersion . package . packageDescription)
                  $ readPackageDescription silent (progName ++ ".cabal")

  return $ toUpperFirst progName ++ " version " ++ version
