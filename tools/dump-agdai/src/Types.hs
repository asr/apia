{-# LANGUAGE UnicodeSyntax #-}

module Types ( printTypes ) where

------------------------------------------------------------------------------
-- Haskell imports

import qualified Data.HashMap.Strict as HashMap ( toList )

import Data.Int  ( Int32 )
import Data.List ( sortBy )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Abstract.Name
  ( Name(nameBindingSite)
  , QName(qnameName)
  )

import Agda.Syntax.Internal ( Type )

import Agda.TypeChecking.Monad.Base
  ( Definition
  , Definitions
  , defType
  , Interface(iSignature)
  , Signature(sigDefinitions)
  )

import Agda.Syntax.Position
  ( Interval'(iStart)
  , Position'(posLine)
  , rangeToInterval
  )

------------------------------------------------------------------------------
-- Auxiliary functions

-- From Data.Function. This function is not in Haskell2010.
on ∷ (b → b → c) → (a → b) → a → a → c
(.*.) `on` f = \x y → f x .*. f y

qNameLine ∷ QName → Int32
qNameLine qName =
  case rangeToInterval $ nameBindingSite $ qnameName qName of
    Nothing → error "qNameLine"
    Just i  → posLine $ iStart i

-- We sort the 'QName's by its position in the Agda module.
myQNameCompare ∷ QName → QName → Ordering
myQNameCompare = compare `on` qNameLine

myQNameDefinitionCompare ∷ (QName, Definition) → (QName, Definition) → Ordering
myQNameDefinitionCompare = myQNameCompare `on` fst

------------------------------------------------------------------------------

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
