-----------------------------------------------------------------------------
-- |
-- Module      : Apia.Dump
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Dump Agda interface file information to stdout.
-----------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Dump
  ( dumpAgdai
  , dumpQNames
  ) where

------------------------------------------------------------------------------

import Agda.Syntax.Abstract.Name ( QName, qnameToConcrete )
import Agda.Syntax.Internal      ( Type )

import Agda.TypeChecking.Monad.Base
  ( Definition
  , Definitions
  , defType
  , Interface(iSignature)
  , Signature(sigDefinitions)
  )

import Agda.Utils.Pretty ( prettyShow )

import Apia.Monad.Base ( T )

import Apia.Utils.AgdaAPI.Interface
  ( qNameConcreteNameRange
  , qNameLine
  , qNameNameBindingSiteRange
  , readInterface
  )

import qualified Data.HashMap.Strict as HashMap ( toList )

import Data.Function ( on )
import Data.List     ( sortBy )

import Control.Monad.IO.Class ( MonadIO(liftIO) )

------------------------------------------------------------------------------
-- We sort the 'QName's by its position in the Agda module.
compareQName ∷ QName → QName → Ordering
compareQName = compare `on` qNameLine

compareQNameDefinition ∷ (QName, Definition) → (QName, Definition) → Ordering
compareQNameDefinition = compareQName `on` fst

dumpQNameInformation ∷ (QName, Definition) → T ()
dumpQNameInformation (qName, def) = do

  let ty ∷ Type
      ty = defType def

  liftIO $ putStrLn $ "Qname: " ++ (prettyShow . qnameToConcrete) qName
  liftIO $ putStrLn $ "Type: "  ++ show ty
  liftIO $ putStrLn $ "Concrete name range: "
                      ++ (show . qNameConcreteNameRange) qName
  liftIO $ putStrLn $ "nameBindingSite range: "
                      ++ (show . qNameNameBindingSiteRange) qName ++ "\n"

-- | Print 'QName's information to stdout.
dumpQNames ∷ FilePath → T ()
dumpQNames file = do

  i ← readInterface file

  let defs ∷ Definitions
      defs = sigDefinitions $ iSignature i

  mapM_ dumpQNameInformation $ sortBy compareQNameDefinition $ HashMap.toList defs

-- | Print the Agda interface file to stdout.
dumpAgdai ∷ FilePath → T ()
dumpAgdai file =  liftIO . putStrLn . prettyShow =<< readInterface file
