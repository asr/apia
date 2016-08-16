
-- | Reports via the @--verbose@ option.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Monad.Reports
  ( reportDLn
  , reportS
  , reportSLn
  , VerboseKey  -- Required by Haddock.
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Impossible ( Impossible (Impossible), throwImpossible )

import qualified Agda.Utils.Trie as Trie ( lookupPath )

import Agda.Utils.List ( wordsBy )

import Apia.Monad.Base        ( askTOpt, T )
import Apia.Options           ( Options(optVerbose) )
import Apia.Utils.PrettyPrint ( (<>), Doc, Pretty(pretty), prettyShow )

import qualified Data.Text as T ( pack )

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
reportS k n s = verboseS k n $ putStr $ T.pack (s ++ "\n")

-- | Print debug information via the @--verbose@ option.
reportSLn ∷ VerboseKey → Int → String → T ()
reportSLn k n s = verboseS k n $ putStrLn $ T.pack (s ++ "\n")

-- | Print debug information via the @--verbose@ option.
reportDLn ∷ VerboseKey → Int → Doc → T ()
reportDLn k n s =
  verboseS k n $ putStrLn $ T.pack $ prettyShow (s <> pretty "\n")
