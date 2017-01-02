
-- | Utilities on directory manipulation.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Directory
  ( checkExecutable
  , equalFiles
  , notEqualFiles
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Maybe ( caseMaybeM )
import Agda.Utils.Monad ( ifM )

import qualified Apia.Utils.Except as E

import Apia.Utils.PrettyPrint ( Doc )

import qualified Data.ByteString.Lazy as BL ( readFile )

import System.Directory ( findExecutable )

------------------------------------------------------------------------------

-- | @checkExecutable file msg@ throws an exception with message @msg@
-- if the executable @file@ is missing.
checkExecutable ∷ MonadIO m ⇒ FilePath → Doc → E.ExceptT Doc m ()
checkExecutable file msg =
  caseMaybeM (liftIO $ findExecutable file)
             (E.throwE msg)
             (\_ → return ())

-- | Return 'True' if the files are equals, otherwise the function
-- returns 'False'.
equalFiles ∷ FilePath → FilePath → IO Bool
equalFiles f1 f2 = liftM2 (==) (BL.readFile f1) (BL.readFile f2)

-- | Return 'True' if the files are different, otherwise the function
-- returns 'False'.
notEqualFiles ∷ FilePath → FilePath → IO Bool
notEqualFiles f1 f2 = ifM (equalFiles f1 f2) (return False) (return True)
