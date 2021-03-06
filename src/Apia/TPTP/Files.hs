
-- | Creation of the TPTP files.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.TPTP.Files ( createConjectureTPTPFile ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name
  ( Name(nameConcrete)
  , QName(qnameName)
  , qnameToConcrete
  )

import Agda.Syntax.Common ( TPTPRole )

import Agda.Syntax.Concrete.Name ( nameStringParts )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( whenM )
import Agda.Utils.Pretty     ( prettyShow )

import Apia.Monad.Base    ( askTOpt, T )
import Apia.Monad.Reports ( reportS, reportSLn )

import Apia.Options ( Options(optInputFile, optOnlyFiles, optOutputDir) )

import Apia.TPTP.ConcreteSyntax ( ToTPTP(toTPTP) )

import Apia.TPTP.Types
  ( AF(AFor)
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

import Apia.Utils.List        ( duplicate )
import Apia.Utils.Monad       ( die )
import Apia.Utils.PrettyPrint ( (<>), scquotes, spaces )
import Apia.Utils.Text        ( (+++), toUpperFirst )

import Control.Exception ( catch, IOException )

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory   ( createDirectoryIfMissing )
import System.Environment ( getProgName )

import System.FilePath
  ( (</>)
  , addExtension
  , dropExtension
  )

#include "undefined.h"

------------------------------------------------------------------------------

class AsciiName a where
  asciiName ∷ a → FilePath

instance AsciiName Char where
  asciiName c
    | c == '-' = [c]
    | c `elem` ("._" :: String) = __IMPOSSIBLE__
    -- The character is a subscript digit (i.e. ₀, ₁, ..., ₉).
    | ord c `elem` [8320 .. 8329] = [chr (ord c - 8272)]
    | isDigit c || isAsciiUpper c || isAsciiLower c = [c]
    | otherwise = show $ ord c

-- Requires @TypeSynonymInstances@.
instance AsciiName String where
  asciiName = concatMap asciiName

fofExt ∷ String
fofExt = ".fof"

commentLine ∷ Text
commentLine = "%" +++ T.replicate 77 (T.singleton '-') +++ "\n"

commentLineLn ∷ Text
commentLineLn = commentLine +++ "\n"

conjectureHeader ∷ IO Text
conjectureHeader = do
  progName ← fmap (toUpperFirst . T.pack) getProgName
  return $
    commentLine
    +++ "% This file was automatically generated by "
    +++ progName +++ ".\n"
    +++ commentLineLn

conjectureFooter ∷ Text
conjectureFooter = commentLine +++ "% End TPTP file.\n"

agdaOriginalTerm ∷ QName → TPTPRole → Text
agdaOriginalTerm qName role =
  "% The Agda term was:\n"
  +++ "% Name: " +++ (T.pack . prettyShow . qnameToConcrete) qName +++ "\n"
  +++ "% Role: " +++ (T.pack . prettyShow) role +++ "\n"
  +++ "% Line: " +++ line +++ "\n"
  where
  line :: Text
  line = case qNameLine qName of
    Just l  → T.pack $ prettyShow l
    Nothing → "N/A"

addRole ∷ FilePath → AF → IO ()
addRole file af@(AFor qName afRole _) = do
  T.appendFile file $ agdaOriginalTerm qName afRole
  T.appendFile file $ toTPTP af

addRoles ∷ FilePath → [AF] → Text → IO ()
addRoles _    []  _   = return ()
addRoles file afs str = do
  let header, footer ∷ Text
      header = commentLine +++ "% The " +++ str +++ ".\n\n"
      footer = "% End " +++ str +++ ".\n\n"

  T.appendFile file header
  mapM_ (addRole file) $ sort afs
  T.appendFile file footer

createConjectureFile ∷ FilePath → GeneralRoles → ConjectureSet → T FilePath
createConjectureFile file generalRoles conjectureSet = do

  when (duplicate (axioms generalRoles))     (__IMPOSSIBLE__)
  when (duplicate (defsAxioms generalRoles)) (__IMPOSSIBLE__)
  when (duplicate (hints generalRoles))      (__IMPOSSIBLE__)
  when (duplicate (defsHints generalRoles))  (__IMPOSSIBLE__)

  when (duplicate (defsConjecture conjectureSet))       (__IMPOSSIBLE__)
  when (duplicate (localHintsConjecture conjectureSet)) (__IMPOSSIBLE__)
  when (duplicate (defsLocalHints conjectureSet))       (__IMPOSSIBLE__)

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
    addRoles file commonDefs "common required definition(s)"
    addRoles file (axioms newGeneralRoles) "general axiom(s)"
    addRoles file (defsAxioms newGeneralRoles)
             "required definition(s) by the general axiom(s)"
    addRoles file (hints newGeneralRoles) "general hint(s)"
    addRoles file (defsHints newGeneralRoles)
             "required definition(s) by the general hint(s)"
    addRoles file (localHintsConjecture  newConjectureSet) "local hint(s)"
    addRoles file (defsLocalHints newConjectureSet)
             "required definition(s) by the local hint(s)"
    addRoles file (defsConjecture newConjectureSet)
             "required definition(s) by the conjecture"
    addRoles file [ theConjecture newConjectureSet ] "conjecture"
    T.appendFile file conjectureFooter

  whenM (askTOpt optOnlyFiles) $ reportS "" 1 $ "Created " ++ file

  return file

tptpFileName ∷ ConjectureSet → T FilePath
tptpFileName conjectureSet = do
  -- To avoid clash names with the terms inside a where clause, we
  -- added the line number where the term was defined to the file
  -- name.
  outputDir ← askTOpt optOutputDir
  inputFile ← fromMaybe (__IMPOSSIBLE__) <$> askTOpt optInputFile

  reportSLn "tptpFileName" 20 $ "outputDir: " ++ outputDir
  reportSLn "tptpFileName" 20 $ "inputFile: " ++ inputFile

  let qName ∷ QName
      qName = case theConjecture conjectureSet of
                AFor _qName _ _ → _qName

      finalDir ∷ FilePath
      finalDir = outputDir </> dropExtension inputFile

  liftIO $ createDirectoryIfMissing True finalDir `catch`
    \ (_ :: IOException) →
      die $ "could not create the" <> spaces (scquotes finalDir) <> "directory"

  reportSLn "tptpFileName" 20 $
    "Qname's concrete name range: " ++ (show . qNameConcreteNameRange) qName

  reportSLn "tptpFileName" 20 $
    "Qname's nameBindingSite range: "
    ++ (show . qNameNameBindingSiteRange) qName

  let f ∷ FilePath
      f = finalDir </>
            maybe (__IMPOSSIBLE__) show (qNameLine qName)
            ++ "-"
            ++ asciiName ((concat . nameStringParts . nameConcrete . qnameName) qName)

      file ∷ FilePath
      file = addExtension f fofExt

  reportSLn "tptpFileName" 20 $ "Creating " ++ show file

  return file

-- | The 'createConjectureTPTPFile' function creates a TPTP file with a
-- conjecture.
createConjectureTPTPFile ∷ GeneralRoles → ConjectureSet → T FilePath
createConjectureTPTPFile generalRoles conjectureSet = do
  file ← tptpFileName conjectureSet
  createConjectureFile file generalRoles conjectureSet
