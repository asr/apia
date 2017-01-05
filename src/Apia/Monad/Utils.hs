-- | Utilities on the translation monad.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Utils
  ( doesFileExistErr
  , findExecutableErr
  ) where

import Apia.Prelude

import Agda.Utils.FileName ( doesFileExistCaseSensitive )
import Agda.Utils.Maybe    ( whenNothingM )
import Agda.Utils.Monad    ( unlessM )

import Apia.Monad.Base
  ( T
  , tErr
  , TErr
  )

import System.Directory ( findExecutable )

------------------------------------------------------------------------------

-- | @findExecutableErr file err@ throws exception @err@ if the
-- executable @file@ is missing.
findExecutableErr ∷ FilePath → TErr → T ()
findExecutableErr file err =
  whenNothingM (liftIO $ findExecutable file)
               (tErr err)

-- | @doesFileExistErr file err@ throws exception @err@ if @file@ do
-- not exist.
doesFileExistErr ∷ FilePath → TErr → T ()
doesFileExistErr file err =
  unlessM (liftIO $ doesFileExistCaseSensitive file)
          (tErr err)
