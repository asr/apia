------------------------------------------------------------------------------
-- |
-- Module      : ApiaTPTP.Files
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Creation of the TPTP files.
------------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Apia.TPTP.Files ( createConjectureFile ) where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad           ( when )
import Control.Monad.IO.Class  ( MonadIO(liftIO) )

import Data.Char ( chr, isAsciiUpper, isAsciiLower, isDigit, ord )
import Data.List ( sort )

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory   ( createDirectoryIfMissing )
import System.Environment ( getProgName )
import System.FilePath    ( (</>), addExtension )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Abstract.Name
  ( mnameToConcrete
  , Name(nameConcrete)
  , QName(qnameModule, qnameName)
  , qnameToConcrete
  )

import Agda.Syntax.Common ( ATPRole )

import Agda.Syntax.Concrete.Name
  ( moduleNameToFileName
  , nameStringParts
  , toTopLevelModuleName
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( whenM )
import Agda.Utils.Pretty     ( prettyShow )

------------------------------------------------------------------------------
-- Apia imports

import Apia.Monad.Base          ( askTOpt, T )
import Apia.Monad.Reports       ( reportS, reportSLn )
import Apia.Options             ( Options(optOnlyFiles, optOutputDir) )
import Apia.TPTP.ConcreteSyntax ( ToTPTP(toTPTP) )

import Apia.TPTP.Types
  ( AF(AF)
  , allRequiredDefs
  , ConjectureSet(defsConjecture
                 , defsLocalHints
                 , localHintsConjecture
                 , theConjecture
                 )
  , commonRequiredDefs
  , dropCommonRequiredDefs
  , GeneralRoles(axioms, defsAxioms, defsHints, hints)
  )

import Apia.Utils.AgdaAPI.Interface
 ( qNameConcreteNameRange
 , qNameLine
 , qNameNameBindingSiteRange
 )

import Apia.Utils.List   ( duplicate )
import Apia.Utils.String ( removeString )
import Apia.Utils.Text   ( (+++), toUpperFirst )

#include "undefined.h"

------------------------------------------------------------------------------

class AsciiName a where
  asciiName ∷ a → FilePath

instance AsciiName Char where
  asciiName c
    | c == '-' = [c]
    | c `elem` "._" = __IMPOSSIBLE__
    -- The character is a subscript digit (i.e. ₀, ₁, ..., ₉).
    | ord c `elem` [8320 .. 8329] = [chr (ord c - 8272)]
    | isDigit c || isAsciiUpper c || isAsciiLower c = [c]
    | otherwise = show $ ord c

-- Requires @TypeSynonymInstances@.
instance AsciiName String where
  asciiName = concatMap asciiName

tptpExt ∷ String
tptpExt = ".tptp"

commentLine ∷ Text
commentLine = "%-----------------------------------------------------------------------------\n"

commentLineLn ∷ Text
commentLineLn = commentLine +++ "\n"

conjectureHeader ∷ IO Text
conjectureHeader = do
  progName ← fmap (toUpperFirst . T.pack) getProgName
  return $
    commentLine
    +++ "% This file was generated automatically by "
    +++ progName +++ ".\n"
    +++ commentLineLn

conjectureFooter ∷ Text
conjectureFooter = commentLine +++ "% End TPTP file.\n"

agdaOriginalTerm ∷ QName → ATPRole → Text
agdaOriginalTerm qName role =
  "% The original Agda term was:\n"
  +++ "% Name: " +++ (T.pack . prettyShow . qnameToConcrete) qName +++ "\n"
  +++ "% Role: " +++ (T.pack . show) role +++ "\n"
  +++ "% ATP pragma line: " +++ (T.pack . prettyShow . qNameLine) qName +++ "\n"

addRole ∷ AF → FilePath → IO ()
addRole af@(AF qName afRole _) file = do
  T.appendFile file $ agdaOriginalTerm qName afRole
  T.appendFile file $ toTPTP af

addRoles ∷ [AF] → FilePath → Text → IO ()
addRoles []  _    _   = return ()
addRoles afs file str = do
  let header, footer ∷ Text
      header = commentLine +++ "% The " +++ str +++ ".\n\n"
      footer = "% End " +++ str +++ ".\n\n"

  T.appendFile file header
  mapM_ (`addRole` file) $ sort afs
  T.appendFile file footer

-- | The function 'createConjectureFile' creates a TPTP file with a
-- conjecture.
createConjectureFile ∷ GeneralRoles → ConjectureSet → T FilePath
createConjectureFile generalRoles conjectureSet = do
  -- To avoid clash names with the terms inside a where clause, we
  -- added the line number where the term was defined to the file
  -- name.

  when (duplicate (axioms generalRoles))     (__IMPOSSIBLE__)
  when (duplicate (defsAxioms generalRoles)) (__IMPOSSIBLE__)
  when (duplicate (hints generalRoles))      (__IMPOSSIBLE__)
  when (duplicate (defsHints generalRoles))  (__IMPOSSIBLE__)

  when (duplicate (defsConjecture conjectureSet))       (__IMPOSSIBLE__)
  when (duplicate (localHintsConjecture conjectureSet)) (__IMPOSSIBLE__)
  when (duplicate (defsLocalHints conjectureSet))       (__IMPOSSIBLE__)

  outputDir ← askTOpt optOutputDir

  let qName ∷ QName
      qName = case theConjecture conjectureSet of
                AF _qName _ _ → _qName

      moduleDir ∷ FilePath
      moduleDir = ((`moduleNameToFileName` [])
                   . toTopLevelModuleName
                   . mnameToConcrete
                   . qnameModule) qName

      -- We removed the "/_"s in the module name produced by Agda when
      -- the qName is inside a where clause.
      finalDir ∷ FilePath
      finalDir = outputDir </> removeString "/_" moduleDir

  liftIO $ createDirectoryIfMissing True finalDir

  reportSLn "createConjectureFile" 20 $ "Final dir: " ++ finalDir

  reportSLn "createConjectureFile" 20 $
    "Qname's concrete name range: " ++ (show . qNameConcreteNameRange) qName

  reportSLn "createConjectureFile" 20 $
    "Qname's nameBindingSite range: "
    ++ (show . qNameNameBindingSiteRange) qName

  let f ∷ FilePath
      f = finalDir </>
            (show . qNameLine) qName
            ++ "-"
            ++ asciiName ((concat . nameStringParts . nameConcrete . qnameName) qName)

      file ∷ FilePath
      file = addExtension f tptpExt

  reportSLn "createConjectureFile" 20 $ "Creating " ++ show file

  let commonDefs ∷ [AF]
      commonDefs = commonRequiredDefs generalRoles conjectureSet

  reportSLn "createConjectureFile" 20 $ "commonDefs: " ++ show commonDefs

  let newGeneralRoles  ∷ GeneralRoles
      newConjectureSet ∷ ConjectureSet
      (newGeneralRoles, newConjectureSet) =
        dropCommonRequiredDefs generalRoles conjectureSet

  when (duplicate (allRequiredDefs newGeneralRoles newConjectureSet))
       (__IMPOSSIBLE__)

  liftIO $ do
    conjectureH ← conjectureHeader
    T.writeFile file conjectureH
    addRoles commonDefs file "common required definition(s)"
    addRoles (axioms newGeneralRoles) file "general axiom(s)"
    addRoles (defsAxioms newGeneralRoles) file
             "required ATP definition(s) by the general axiom(s)"
    addRoles (hints newGeneralRoles) file "general hint(s)"
    addRoles (defsHints newGeneralRoles) file
             "required ATP definition(s) by the general hint(s)"
    addRoles (localHintsConjecture  newConjectureSet) file "local hint(s)"
    addRoles (defsLocalHints newConjectureSet) file
             "required ATP definition(s) by the local hint(s)"
    addRoles (defsConjecture newConjectureSet) file
             "required ATP definition(s) by the conjecture"
    addRoles [theConjecture newConjectureSet] file "conjecture"
    T.appendFile file conjectureFooter

  whenM (askTOpt optOnlyFiles) $ reportS "" 1 $ "Created " ++ file

  return file
