-----------------------------------------------------------------------------
-- |
-- Module      : Apia.Monad.Reports
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Reports via the @--verbose@ option.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Reports
  ( reportS
  , reportSLn
  , VerboseKey  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad          ( when )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Utils.Impossible ( Impossible (Impossible), throwImpossible )

import qualified Agda.Utils.Trie as Trie ( lookupPath )

import Agda.Utils.List ( wordsBy )

------------------------------------------------------------------------------
-- Apia imports

import Apia.Monad.Base ( askTOpt, T )
import Apia.Options    ( Options(optVerbose) )

#include "undefined.h"

-----------------------------------------------------------------------------
-- Nice way to report things via the @--verbose@ option. Adapted from
-- @Agda.TypeChecking.Monad.Options@.

-- | Key for the @--verbose@ option.
type VerboseKey = String

-- Precondition: The level must be non-negative.
verboseS ∷ VerboseKey → Int → T () → T ()
verboseS k n action | n < 0     =  __IMPOSSIBLE__
                    | otherwise = do
  t ← askTOpt optVerbose
  let ks ∷ [String]
      ks = wordsBy (`elem` ".:") k

      m ∷ Int
      m = maximum $ 0 : Trie.lookupPath ks t
  when (n <= m) action

-- | Print debug information via the @--verbose@ option.
reportS ∷ VerboseKey → Int → String → T ()
reportS k n s = verboseS k n $ liftIO $ putStr (s ++ "\n")

-- | Print debug information via the @--verbose@ option.
reportSLn ∷ VerboseKey → Int → String → T ()
reportSLn k n s = verboseS k n $ liftIO $ putStrLn (s ++ "\n")
