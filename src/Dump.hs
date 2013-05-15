-----------------------------------------------------------------------------
-- |
-- Module      : Dump
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Dump Agda interface file information to stdout.
-----------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Dump ( dumpAgdai, dumpTypes )
where

------------------------------------------------------------------------------
-- Haskell imports

import qualified Data.HashMap.Strict as HashMap ( toList )

import Data.Function ( on )
import Data.List     ( sortBy )

import Control.Monad.Trans ( MonadIO(liftIO) )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Abstract.Name ( QName )
import Agda.Syntax.Internal      ( Type )

import Agda.TypeChecking.Monad.Base
  ( Definition
  , Definitions
  , defType
  , Interface(iSignature)
  , Signature(sigDefinitions)
  )

------------------------------------------------------------------------------
-- Local imports

import AgdaInternal.Interface ( qNameLine, readInterface )
import Monad.Base             ( T )

------------------------------------------------------------------------------
-- We sort the 'QName's by its position in the Agda module.
compareQName ∷ QName → QName → Ordering
compareQName = compare `on` qNameLine

compareQNameDefinition ∷ (QName, Definition) → (QName, Definition) → Ordering
compareQNameDefinition = compareQName `on` fst

dumpQNameType ∷ (QName, Definition) → T ()
dumpQNameType (qName, def) = do

  let ty ∷ Type
      ty = defType def

  liftIO $ putStrLn $ "Qname: " ++ show qName
  liftIO $ putStrLn $ "Type: "  ++ show ty ++ "\n"

-- | Print type information to stdout.
dumpTypes ∷ FilePath → T ()
dumpTypes file = do

  i ← readInterface file

  let defs ∷ Definitions
      defs = sigDefinitions $ iSignature i

  mapM_ dumpQNameType $ sortBy compareQNameDefinition $ HashMap.toList defs

-- | Print the Agda interface file to stdout.
dumpAgdai ∷ FilePath → T ()
dumpAgdai file = do
  i ← readInterface file
  liftIO $ print i
