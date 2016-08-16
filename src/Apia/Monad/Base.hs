
-- | The translation monad.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Base
  ( askTOpt
  , getTATPs
  , getTDefs
  , getTVars
  , isTVarsEmpty
  , modifyTATPs
  , modifyTDefs
  , newTVar
  , popTVar
  , pushTNewVar
  , pushTVar
  , runT
  , T
  , TState  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.TypeChecking.Monad.Base ( Definitions )
import Agda.Utils.Impossible        ( Impossible(Impossible), throwImpossible )

import Apia.Common ( ATP )
import Apia.Monad.Environment ( env )
import Apia.Options           ( Options )

import qualified Apia.Utils.Except as E

import Apia.Utils.Name        ( freshName )
import Apia.Utils.PrettyPrint ( Doc )

import Control.Monad.Trans.Class ( MonadTrans(lift) )

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

#include "undefined.h"

------------------------------------------------------------------------------
-- | The translation monad state.

-- See note [@OptionsPragma@].
data TState = TState
  { tDefs ∷ Definitions  -- ^ Agda definitions.
  , tVars ∷ [String]     -- ^ Variables names.
  , tATPs ∷ [ATP]        -- ^ Selected ATPs.
  }

-- The initial state.
initTState ∷ TState
initTState = TState { tDefs = HashMap.empty
                    , tVars = []
                    , tATPs = []
                    }

-- | The translation monad.
type T = E.ExceptT Doc (StateT TState (ReaderT Options IO))

-- | Running the translation monad.
runT ∷ T a → IO (Either Doc a)
runT ta = env >>= runReaderT (evalStateT (E.runExceptT ta) initTState)

-- | Return 'True' if the list of variables in the translation monad
-- state is empty.
isTVarsEmpty ∷ T Bool
isTVarsEmpty = lift $ fmap (null . tVars) get

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

-- | Get the ATPs from the translation monad state.
getTATPs ∷ T [ATP]
getTATPs = lift $ fmap tATPs get

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

-- | Modify the ATPs in the translation monad state.
modifyTATPs ∷ [ATP] → T ()
modifyTATPs atps = lift $ modify $ \s → s { tATPs = atps }

-- | Modify the Agda 'Definitions' in the translation monad state.
modifyTDefs ∷ Definitions → T ()
modifyTDefs defs = lift $ modify $ \s → s { tDefs = defs }
