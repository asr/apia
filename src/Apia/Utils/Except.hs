
-- | Wrappers for Control.Monad.Trans.Except

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Except
 ( catchE
 , ExceptT
 , throwE
 , runExceptT
 ) where

------------------------------------------------------------------------------

#if MIN_VERSION_transformers(0,4,1)
import Control.Monad.Trans.Except ( catchE, ExceptT, throwE, runExceptT )
#else
import Control.Monad.Trans.Error
  ( catchError
  , Error
  , ErrorT(runErrorT)
  , throwError
  )
#endif

------------------------------------------------------------------------------

#if !MIN_VERSION_transformers(0,4,1)
-- | 'ExcepT' type using transformers 0.3.*.
type ExceptT = ErrorT

-- | 'catchE' function using transformers 0.3.*.
catchE ∷ (Monad m, Error e) ⇒
         ExceptT e m a → (e → ExceptT e m a) → ExceptT e m a
catchE = catchError

-- | 'runExcept' function using transformers 0.3.*.
runExceptT ∷  ExceptT e m a → m (Either e a)
runExceptT = runErrorT

-- | 'throwE' function using transformers 0.3.*.
throwE ∷ (Monad m, Error e) ⇒ e → ExceptT e m a
throwE = throwError
#endif
