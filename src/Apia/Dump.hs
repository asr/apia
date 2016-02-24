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
  ( dumpTypes
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name ( QName, qnameToConcrete )
import Agda.Syntax.Internal      ( Type )

import Agda.TypeChecking.Monad.Base
  ( Definition
  , Definitions
  , defType
  , Interface(iSignature)
  , sigDefinitions
  )

import Agda.Utils.Lens   ( (^.) )
import Agda.Utils.Pretty ( prettyShow )

import Apia.Monad.Base ( T )

import Apia.Utils.AgdaAPI.Interface
  ( qNameConcreteNameRange
  , qNameLine
  , qNameNameBindingSiteRange
  , readInterface
  )

import qualified Data.HashMap.Strict as HashMap ( toList )

import qualified Data.Text as T ( pack )

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

  putStrLn $ T.pack $ "Qname: " ++ (prettyShow . qnameToConcrete) qName
  putStrLn $ T.pack $ "Type: "  ++ show ty
  putStrLn $ T.pack $ "Concrete name range: "
                      ++ (show . qNameConcreteNameRange) qName
  putStrLn $ T.pack $ "nameBindingSite range: "
                      ++ (show . qNameNameBindingSiteRange) qName ++ "\n"

-- | Print Agda types information to stdout.
dumpTypes ∷ FilePath → T ()
dumpTypes file = do

  i ← readInterface file

  let defs ∷ Definitions
      defs = iSignature i ^. sigDefinitions

  mapM_ dumpQNameInformation $ sortBy compareQNameDefinition $ HashMap.toList defs
