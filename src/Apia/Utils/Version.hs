------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.Version
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities related to representation of versions.
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Version ( progNameVersion ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Applicative ( (<$>) )

import Data.List ( intercalate )

import Distribution.Package                  ( PackageIdentifier(pkgVersion) )
import Distribution.PackageDescription       ( package , packageDescription )
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Verbosity                ( silent )
import Distribution.Version                  ( Version(Version) )

import System.Environment ( getProgName )

------------------------------------------------------------------------------
-- Apia imports

import Apia.Utils.String ( toUpperFirst )

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
  version ← (showVersion . pkgVersion . package . packageDescription)
            <$> readPackageDescription silent (progName ++ ".cabal")

  return $ toUpperFirst progName ++ " version " ++ version