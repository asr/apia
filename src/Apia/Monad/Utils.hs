-- | Utilities on the translation monad.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Utils
  ( findExecutableErr
  ) where

import Apia.Prelude

import Agda.Utils.Maybe ( whenNothingM )

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
