
-- | Setup.hs module

{-# OPTIONS_GHC -Wall                           #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE ScopedTypeVariables                #-}

import Control.Exception (IOException, try)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Version (Version(versionTags))
import Distribution.Simple
  ( confHook
  , defaultMainWithHooks
  , simpleUserHooks
  )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (ConfigFlags)
import Distribution.Package
  ( PackageIdentifier
    ( PackageIdentifier
    , pkgName
    , pkgVersion
    )
  , PackageName
    ( PackageName
    , unPackageName
    )
  )
import Distribution.PackageDescription
  ( GenericPackageDescription (packageDescription)
  , HookedBuildInfo
  , PackageDescription (package)
  )
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = cHook }

cHook :: (GenericPackageDescription, HookedBuildInfo)
      -> ConfigFlags
      -> IO LocalBuildInfo
cHook (desc, info) cflags = do
  let pdesc :: PackageDescription
      pdesc = packageDescription desc

  finalVersion :: Version <- getVersion $ pkgVersion $ package pdesc
  let newDesc :: GenericPackageDescription
      newDesc = desc {
        packageDescription = pdesc {
            package = PackageIdentifier {
                  pkgName = PackageName { unPackageName = "apia" }
                , pkgVersion = finalVersion }
            }
        }
  confHook simpleUserHooks (newDesc, info) cflags

getVersion :: Version -> IO Version
getVersion version = do
  commit :: Maybe String <- commitInfo
  case commit of
    Nothing  -> return version
    Just rev -> return $ gitVersion version rev

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

commitInfo :: IO (Maybe String)
commitInfo = do
  res <- tryIO $ readProcessWithExitCode "git" ["log", "--format=%h", "-n", "1"] ""
  case res of
    Right (ExitSuccess, hash, _) -> do
      (_, _, _) <- readProcessWithExitCode "git" ["diff", "--quiet"] ""
      return $ Just (init hash)
    _ -> return Nothing

gitVersion :: Version -> String -> Version
gitVersion version hash = version { versionTags = [take 7 hash] }
