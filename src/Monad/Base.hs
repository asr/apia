------------------------------------------------------------------------------
-- |
-- Module      : Monad.Base
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- The translation monad.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Monad.Base
  ( askTOpt
  , catchE
  , getTDefs
  , getTVars
  , isTPragmaOption
  , isTVarsEmpty
  , modifyDefs
  , modifyPragmaOptions
  , newTVar
  , popTVar
  , pushTNewVar
  , pushTVar
  , runT
  , T
  , throwE
  , TState  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad.Trans.Class ( MonadTrans(lift) )

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

import Control.Monad.Trans.Reader ( ask, ReaderT(runReaderT) )

import Control.Monad.Trans.State
  ( evalState
  , evalStateT
  , get
  , modify
  , put
  , StateT
  )

import qualified Data.HashMap.Strict as HashMap ( empty )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Interaction.Options     ( OptionsPragma )
import Agda.TypeChecking.Monad.Base ( Definitions )
import Agda.Utils.Impossible        ( Impossible(Impossible), throwImpossible )

------------------------------------------------------------------------------
-- Local imports

import Monad.Environment ( env )
import Options           ( Options )
import Utils.Names       ( freshName )

#include "../undefined.h"

------------------------------------------------------------------------------
-- | The translation monad state.

-- See note [@OptionsPragma@].
data TState = TState
  { tDefs          ∷ Definitions    -- ^ Agda definitions.
  , tVars          ∷ [String]       -- ^ Variables names.
  , tPragmaOptions ∷ OptionsPragma  -- ^ Pragma options.
  }

-- The initial state.
initTState ∷ TState
initTState = TState { tDefs          = HashMap.empty
                    , tVars          = []
                    , tPragmaOptions = []
                    }

-- | The translation monad.
#if MIN_VERSION_transformers(0,4,1)
type T = ExceptT String (StateT TState (ReaderT Options IO))
#else
type T = ErrorT String (StateT TState (ReaderT Options IO))
#endif

-- | Running the translation monad.
runT ∷ T a → IO (Either String a)
#if MIN_VERSION_transformers(0,4,1)
runT ta = env >>= runReaderT (evalStateT (runExceptT ta) initTState)
#else
runT ta = env >>= runReaderT (evalStateT (runErrorT ta) initTState)
#endif

-- | Return 'True' if the list of variables in the translation monad
-- state is empty.
isTVarsEmpty ∷ T Bool
isTVarsEmpty = lift $ fmap (null . tVars) get

-- | @isTPragmaOption p@ returns 'True' if the pragma option @p@ is
-- set.
isTPragmaOption ∷ String → T Bool
isTPragmaOption p = do
  state ← lift get
  return (p `elem` tPragmaOptions state)

-- | Fresh variable.
newTVar ∷ T String
newTVar = lift $ fmap (evalState freshName . tVars) get

-- | Pop a variable from the translation monad state.
popTVar ∷ T ()
popTVar = do
  state ← lift get
  case tVars state of
    []       → __IMPOSSIBLE__
    (_ : xs) → lift $ put state { tVars = xs }

-- | Push a variable in the translation monad state.
pushTVar ∷ String → T ()
pushTVar x = do
  state ← lift get
  lift $ put state { tVars = x : tVars state }

-- | Create a fresh variable and push it in the translation monad state.
pushTNewVar ∷ T String
pushTNewVar = newTVar >>= \freshVar → pushTVar freshVar >> return freshVar

-- | Get the Agda 'Definitions' from the translation monad state.
getTDefs ∷ T Definitions
getTDefs = lift $ fmap tDefs get

-- | Ask for a concrete 'Options' from the translation monad
-- environment.
askTOpt ∷ (Options → a) → T a
askTOpt opt = lift $ lift $ fmap opt ask

-- | Get the variables from the translation monad state.
getTVars ∷ T [String]
getTVars = lift $ fmap tVars get

-- | Modify the Agda 'Definitions' in the translation monad state.
modifyDefs ∷ Definitions → T ()
modifyDefs defs = lift $ modify $ \s → s { tDefs = defs }

-- | Modify the 'OptionsPragma' in the translation monad state.
modifyPragmaOptions ∷ OptionsPragma → T ()
modifyPragmaOptions ps = lift $ modify $ \s → s { tPragmaOptions = ps }

#if !(MIN_VERSION_transformers(0,4,1))
-- | 'catchE' function using transformers 0.3.*.
catchE ∷ (Monad m, Error e) ⇒ ErrorT e m a → (e → ErrorT e m a) → ErrorT e m a
catchE = catchError

-- | 'throwE' function using transformers 0.3.*.
throwE ∷ (Monad m, Error e) ⇒ e → ErrorT e m a
throwE = throwError
#endif

------------------------------------------------------------------------------
-- Note [@OptionsPragma@].
--
-- Agda uses the type @[OptionsPragma]@ instead of the type
-- @OptionPragma@ for the pragma options, but it doesn't seem
-- necessary in our case.
