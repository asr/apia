------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.Directory
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities on directory manipulation.
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Directory
  ( checkExecutable
  , equalFiles
  , notEqualFiles
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import qualified Data.ByteString.Lazy as BL ( readFile )

import Control.Monad          ( liftM2 )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import System.Directory ( doesFileExist, findExecutable )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Maybe ( caseMaybeM )
import Agda.Utils.Monad ( ifM )

------------------------------------------------------------------------------
-- Apia imports

import qualified Apia.Utils.Except as E

------------------------------------------------------------------------------

-- | @checkExecutable file msg@ throws an exception with message @msg@
-- if the executable @file@ is missing.
checkExecutable ∷ MonadIO m ⇒ FilePath → String → E.ExceptT String m ()
checkExecutable file msg =
  caseMaybeM (liftIO $ findExecutable file)
             (ifM (liftIO $ doesFileExist file)
                  (return ())
                  (E.throwE msg))
             (\_ → return ())

-- | Return 'True' if the files are equals, otherwise the function
-- returns 'False'.
equalFiles ∷ FilePath → FilePath → IO Bool
equalFiles f1 f2 = liftM2 (==) (BL.readFile f1) (BL.readFile f2)

-- | Return 'True' if the files are different, otherwise the function
-- returns 'False'.
notEqualFiles ∷ FilePath → FilePath → IO Bool
notEqualFiles f1 f2 = liftM2 (/=) (BL.readFile f1) (BL.readFile f2)
