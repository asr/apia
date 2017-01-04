-- | Utilities on the translation monad.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Utils
  ( findExecutableErr
  ) where

import Apia.Prelude

import Agda.Utils.Maybe ( caseMaybeM )

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
  caseMaybeM (liftIO $ findExecutable file)
             (tErr err)
             (\_ → return ())

