
-- | Wrapper for Control.Monad.Except from the mtl package.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Except
 ( catchE
 , ExceptT
 , throwE
 , runExceptT
 ) where

------------------------------------------------------------------------------

import Control.Monad.Except
  ( ExceptT
  , MonadError(catchError, throwError)
  , runExceptT
  )

------------------------------------------------------------------------------

catchE ∷ MonadError e m ⇒ m a → (e → m a) → m a
catchE = catchError

throwE ∷ MonadError e m ⇒ e → m a
throwE = throwError
