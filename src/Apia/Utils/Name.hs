------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.Name
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Utilities on names.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Name
  ( concatName
  , freshName
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Concrete.Name ( NamePart(Id, Hole) )
import Agda.Utils.Impossible     ( Impossible(Impossible), throwImpossible )

import Control.Monad.Trans.State ( get, put, State )

#include "undefined.h"

------------------------------------------------------------------------------
-- Generation of fresh names

chars ∷ String
chars = ['a'..'z']

-- The set of free names for variables @(a, b, ..., aa, ab, ...)@.
freeNames ∷ [String]
freeNames = map (:[]) chars ++ [ s ++ [c] | s ← freeNames, c ← chars ]

findFreeName ∷ [String] → [String] → String
findFreeName _         []       = __IMPOSSIBLE__
findFreeName usedNames (x : xs) =
  if x `elem` usedNames then findFreeName usedNames xs else x

-- | Generate a fresh name.
freshName ∷ State [String] String
freshName = do
  names ← get
  let newName ∷ String
      newName = findFreeName names freeNames
  put $ newName : names
  return newName

------------------------------------------------------------------------------
-- Auxiliary functions

takeId ∷ NamePart → String
takeId Hole         = []
takeId (Id strName) = strName

-- | Use the parts of a name to produce a new function name, e.g. the
-- function @if_then_else_@ is called @ifthenelseq@.
concatName ∷ [NamePart] → String
concatName = concatMap takeId
