
-- | Creation of the TPTP files.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}  -- Implies TypeSynonymInstances.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Apia.TargetLang.Files ( createTargetFile ) where

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

import Apia.Common        ( Lang(SMT2,TPTP), smt2Ext, tptpExt )
import Apia.Monad.Base    ( askTOpt, T )
import Apia.Monad.Reports ( reportS, reportSLn )

import Apia.Options
  ( Options( optInputFile
           , optLang
           , optOnlyFiles
           , optOutputDir
           )
  )

-- import Apia.Monad.Base    ( askTOpt, T )
-- import Apia.Monad.Reports ( reportS, reportSLn )

-- import Apia.Options ( Options(optInputFile, optOnlyFiles, optOutputDir) )

-- import Apia.TPTP.ConcreteSyntax ( ToTPTP(toTPTP) )

import Apia.TargetLang.SMT2ConcreteSyntax ( ToSMT2(toSMT2) )
import Apia.TargetLang.TPTPConcreteSyntax ( ToTPTP(toTPTP) )

import Apia.TargetLang.Types
  ( AF(AExpr, AFor, ATy)
  , allRequiredDefs
  , ConjectureSet( declsConjecture
                 , defsConjecture
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
import Apia.Utils.PrettyPrint ( (<>), spaces, squotes )
import Apia.Utils.Text        ( (+++), toUpperFirst )

import Control.Exception      ( catch, IOException )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

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

commentLine ∷ Text
commentLine = "%" +++ T.replicate 77 (T.singleton '-') +++ "\n"

commentLineLn ∷ Text
commentLineLn = commentLine +++ "\n"

conjectureHeader ∷ Lang → IO Text
conjectureHeader lang =
  case lang of
    TPTP → do
      progName ← fmap (toUpperFirst . T.pack) getProgName
      return $
        commentLine
        +++ "% This file was generated automatically by "
        +++ progName +++ ".\n"
        +++ commentLineLn

    SMT2 → return "(set-logic QF_UF)\n"

conjectureFooter ∷ Lang → Text
conjectureFooter lang =
  case lang of
    TPTP → commentLine +++ "% End TPTP file.\n"
    SMT2 → "(check-sat)\n"

agdaOriginalTerm ∷ QName → TPTPRole → Text
agdaOriginalTerm qName role =
  "% The Agda term was:\n"
  +++ "% Name: " +++ (T.pack . prettyShow . qnameToConcrete) qName +++ "\n"
  +++ "% Role: " +++ (T.pack . prettyShow) role +++ "\n"
  +++ "% Line: " +++ (T.pack . prettyShow . qNameLine) qName +++ "\n"

addRole ∷ FilePath → AF → IO ()
addRole file af@(AFor qName afRole _) = do
  T.appendFile file $ agdaOriginalTerm qName afRole
  T.appendFile file $ toTPTP af
addRole file ae@AExpr{} =
  -- TODO (2016-05-02). T.appendFile file $ agdaOriginalTerm qName afRole
  T.appendFile file (toSMT2 ae +++ "\n")
addRole file at@ATy{} =
  -- TODO (2016-05-02). T.appendFile file $ agdaOriginalTerm qName afRole
  T.appendFile file (toSMT2 at +++ "\n")

addRoles ∷ Lang → FilePath → [AF] → Text → IO ()
addRoles _    _    []  _   = return ()
addRoles lang file afs str = do

  let header ∷ Text
      header = case lang of
        TPTP → commentLine +++ "% The " +++ str +++ ".\n\n"

        -- TODO (2016-05-02).
        SMT2 → T.empty

      footer ∷ Text
      footer = case lang of
        TPTP → "% End " +++ str +++ ".\n\n"

        -- TODO (2016-05-02).
        SMT2 → T.empty

  T.appendFile file header
  mapM_ (addRole file) $ sort afs
  T.appendFile file footer

fileName ∷ ConjectureSet → T FilePath
fileName conjectureSet = do
  -- To avoid clash names with the terms inside a where clause, we
  -- added the line number where the term was defined to the file
  -- name.
  outputDir ← askTOpt optOutputDir
  inputFile ← fromMaybe (__IMPOSSIBLE__) <$> askTOpt optInputFile
  lang      ← askTOpt optLang

  reportSLn "fileName" 20 $ "outputDir: " ++ outputDir
  reportSLn "fileName" 20 $ "inputFile: " ++ inputFile

  let qName ∷ QName
      qName = case theConjecture conjectureSet of
                AFor  _qName _ _ → _qName
                AExpr _qName _ _ → _qName
                _                → __IMPOSSIBLE__

      finalDir ∷ FilePath
      finalDir = outputDir </> dropExtension inputFile

  reportSLn "fileName" 20 $ "Final dir: " ++ finalDir

  liftIO $ createDirectoryIfMissing True finalDir `catch`
    \ (_ :: IOException) →
      die $ "could not create the" <> spaces (squotes finalDir) <> "directory"

  reportSLn "fileName" 20 $
    "Qname's concrete name range: " ++ (show . qNameConcreteNameRange) qName

  reportSLn "fileName" 20 $
    "Qname's nameBindingSite range: "
    ++ (show . qNameNameBindingSiteRange) qName

  let f ∷ FilePath
      f = finalDir </>
            (show . qNameLine) qName
            ++ "-"
            ++ asciiName ((concat . nameStringParts . nameConcrete . qnameName) qName)

      ext ∷ String
      ext = case lang of
              TPTP → tptpExt
              SMT2 → smt2Ext

      file ∷ FilePath
      file = addExtension f ext

  reportSLn "fileName" 20 $ "Creating " ++ show file

  return file

-- | The 'createTargetFile' function creates a TPTP (FOF) or
-- SMT-LIB v2 conjecture file.
createTargetFile ∷ GeneralRoles → ConjectureSet → T FilePath
createTargetFile generalRoles conjectureSet = do
  lang ← askTOpt optLang
  file ← fileName conjectureSet

  reportSLn "createTargetFile" 20 $ "declsConjecture: " ++ show (declsConjecture conjectureSet)

  when (duplicate (axioms generalRoles))     (__IMPOSSIBLE__)
  when (duplicate (defsAxioms generalRoles)) (__IMPOSSIBLE__)
  when (duplicate (hints generalRoles))      (__IMPOSSIBLE__)
  when (duplicate (defsHints generalRoles))  (__IMPOSSIBLE__)

  when (duplicate (declsConjecture conjectureSet))      (__IMPOSSIBLE__)
  when (duplicate (defsConjecture conjectureSet))       (__IMPOSSIBLE__)
  when (duplicate (localHintsConjecture conjectureSet)) (__IMPOSSIBLE__)
  when (duplicate (defsLocalHints conjectureSet))       (__IMPOSSIBLE__)

  let commonDefs ∷ [AF]
      commonDefs = commonRequiredDefs generalRoles conjectureSet

  reportSLn "createTargetFile" 20 $ "commonDefs: " ++ show commonDefs

  let newGeneralRoles  ∷ GeneralRoles
      newConjectureSet ∷ ConjectureSet
      (newGeneralRoles, newConjectureSet) =
        dropCommonRequiredDefs generalRoles conjectureSet

  when (duplicate (allRequiredDefs newGeneralRoles newConjectureSet))
       (__IMPOSSIBLE__)

  liftIO $ do
    conjectureH ← conjectureHeader lang
    T.writeFile file conjectureH
    addRoles lang file commonDefs "common required definition(s)"
    addRoles lang file (axioms newGeneralRoles) "general axiom(s)"
    addRoles lang file (defsAxioms newGeneralRoles)
             "required definition(s) by the general axiom(s)"
    addRoles lang file (hints newGeneralRoles) "general hint(s)"
    addRoles lang file (defsHints newGeneralRoles)
             "required definition(s) by the general hint(s)"
    addRoles lang file (localHintsConjecture  newConjectureSet) "local hint(s)"
    addRoles lang file (defsLocalHints newConjectureSet)
             "required definition(s) by the local hint(s)"
    addRoles lang file (declsConjecture newConjectureSet)
             "required declarations(s) by the conjecture"
    addRoles lang file (defsConjecture newConjectureSet)
             "required definition(s) by the conjecture"
    addRoles lang file [ theConjecture newConjectureSet ] "conjecture"
    T.appendFile file (conjectureFooter lang)

  whenM (askTOpt optOnlyFiles) $ reportS "" 1 $ "Created " ++ file

  return file
