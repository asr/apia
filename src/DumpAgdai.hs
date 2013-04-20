-----------------------------------------------------------------------------
-- |
-- Module      : DumpAgdai
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Dump the Agda interface file and type information to stdout.
-----------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module DumpAgdai ( dumpAgdai )
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

import AgdaInternal.Interface ( myReadInterface, qNameLine )
import Monad.Base             ( T )

------------------------------------------------------------------------------
-- We sort the 'QName's by its position in the Agda module.
myQNameCompare ∷ QName → QName → Ordering
myQNameCompare = compare `on` qNameLine

myQNameDefinitionCompare ∷ (QName, Definition) → (QName, Definition) → Ordering
myQNameDefinitionCompare = myQNameCompare `on` fst

printQNameType ∷ (QName, Definition) → IO ()
printQNameType (qName, def) = do

  let ty ∷ Type
      ty = defType def

  putStrLn $ "Qname: " ++ show qName
  putStrLn $ "Type: "  ++ show ty ++ "\n"

printTypes ∷ Interface → IO ()
printTypes i = do

  putStrLn "\nTypes ***********************************************************"

  let defs ∷ Definitions
      defs = sigDefinitions $ iSignature i

  mapM_ printQNameType $ sortBy myQNameDefinitionCompare $ HashMap.toList defs

-- | Print the Agda interface file and type information to stdout.
dumpAgdai ∷ FilePath → T ()
dumpAgdai agdaFile = do
  i ← myReadInterface agdaFile
  liftIO $ print i
  liftIO $ printTypes i
