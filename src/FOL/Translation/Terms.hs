------------------------------------------------------------------------------
-- |
-- Module      : FOL.Translation.Internal.Terms
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Translation from Agda internal terms to first-order logic formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module FOL.Translation.Terms
  ( termToFormula
  , termToFOLTerm
  )
  where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad ( liftM2, when )
import Control.Monad.Error ( MonadError(throwError) )

import Data.List ( foldl' )

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Abstract.Name ( Name(nameConcrete, nameId) , QName(QName) )

import Agda.Syntax.Common
  ( Arg(Arg, argInfo, unArg)
  , ArgInfo(ArgInfo, argInfoHiding)
  , Dom(Dom, unDom)
  , Hiding(Hidden, NotHidden)
  , NameId(NameId)
  , Nat
  )

import qualified Agda.Syntax.Concrete.Name as C
  ( Name(Name, NoName)
  , NamePart(Id, Hole)
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , Arg
  , Args
  , Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Term(Con, Def, Lam, Pi, Sort, Var)
  , Type(El)
  )

import Agda.Syntax.Position  ( noRange )
import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( ifM )

------------------------------------------------------------------------------
-- Local imports

import AgdaInternal.Interface ( isATPDefinition, qNameDefinition )

import FOL.Constants
  ( folTrue, folFalse, folNot, folAnd, folOr
  , folImplies, folEquiv, folExists, folForAll, folEquals
  )

import FOL.Primitives       ( app, equal, predicateTranslation )
import FOL.Translation.Name ( concatName )

import {-# source #-} FOL.Translation.Types
  ( domTypeToFormula
  , typeToFormula
  )

import FOL.Types
  ( FOLFormula(TRUE, FALSE, Predicate, Not, And, Or, Implies, Equiv, ForAll, Exists)
  , FOLTerm(FOLFun, FOLVar)
  )

import Monad.Base
  ( askTOpt
  , getTVars
  , isTPragmaOption
  , newTVar
  , popTVar
  , pushTNewVar
  , T
  )

import Monad.Reports ( reportSLn )

import Options ( Options(optWithFnApp, optWithoutPSymbols) )

#include "../../undefined.h"

------------------------------------------------------------------------------

universalQuantificationMsg ∷ String → String
universalQuantificationMsg p =
  "Use the Agda option " ++ "`" ++ p ++ "'" ++ " in your Agda file "
  ++ "for the translation of first-order logic universal quantified "
  ++ entities
  where
    entities ∷ String
    entities =
      case p of
        "--universal-quantified-formulae"                → "formulae"
        "--universal-quantified-functions"               → "functions"
        "--universal-quantified-propositional-functions" → "propositional functions"
        _                                                → __IMPOSSIBLE__

qName2String ∷ QName → T String
qName2String qName@(QName _ name) = do
  def ← qNameDefinition qName

  -- See note [Unique name].
  if isATPDefinition def
    then do
      let qNameId ∷ NameId
          qNameId = nameId name

      reportSLn "qName2String" 20 $ "qNameId : " ++ show qNameId

      case qNameId of
        NameId x i → return $ show (nameConcrete name) ++ "_"
                              ++ show x ++ "_"
                              ++ show i
    else return $ show $ nameConcrete name

argTermToFormula ∷ I.Arg Term → T FOLFormula
argTermToFormula Arg {argInfo = info, unArg = t} =
  case info of
    ArgInfo { argInfoHiding = NotHidden } → termToFormula t
    _                                     → __IMPOSSIBLE__

argTermToFOLTerm ∷ I.Arg Term → T FOLTerm
argTermToFOLTerm Arg {argInfo = info, unArg = t} =
  case info of
    ArgInfo { argInfoHiding = Hidden }    → termToFOLTerm t
    ArgInfo { argInfoHiding = NotHidden } → termToFOLTerm t
    _                                     → __IMPOSSIBLE__

binConst ∷ (FOLFormula → FOLFormula → FOLFormula) →
           I.Arg Term →
           I.Arg Term →
           T FOLFormula
binConst op arg1 arg2 =
  liftM2 op (argTermToFormula arg1) (argTermToFormula arg2)


-- Translation of predicates.
predicate ∷ QName → Args → T FOLFormula
predicate qName args = do
  folName ← qName2String qName
  termsFOL ← mapM argTermToFOLTerm args

  case length args of
    0 → __IMPOSSIBLE__
    _ → ifM (askTOpt optWithoutPSymbols)
            -- Direct translation.
            (return $ Predicate folName termsFOL)
            -- Translation using Koen's suggestion.
            (return $ predicateTranslation (FOLFun folName []) termsFOL)

predicateLogicalScheme ∷ [String] → Nat → Args → T FOLFormula
predicateLogicalScheme vars n args = do
  let var ∷ String
      var = vars !! n

  case length args of
    0 → __IMPOSSIBLE__
    _ → fmap (predicateTranslation (FOLVar var)) (mapM argTermToFOLTerm args)

-- | Translate an Agda internal 'Term' to a first-order logic formula
-- 'FOLFormula'.
termToFormula ∷ Term → T FOLFormula
termToFormula term@(Def qName@(QName _ name) args) = do
  reportSLn "t2f" 10 $ "termToFormula Def:\n" ++ show term

  let cName ∷ C.Name
      cName = nameConcrete name

  case cName of
    C.NoName{} → __IMPOSSIBLE__

    C.Name _ [] → __IMPOSSIBLE__

    C.Name{} →
     case args of
       [] | isCNameFOLConst folTrue  → return TRUE

          | isCNameFOLConst folFalse → return FALSE

          | otherwise → do
            -- In this guard we translate 0-ary predicates,
            --
            -- e.g.  @P : Set@.
            folName ← qName2String qName
            return $ Predicate folName []

       (a : [])
         | isCNameFOLConstHoleRight folNot → fmap Not (argTermToFormula a)

         | isCNameFOLConst folExists ||
           isCNameFOLConst folForAll → do
             fm ← argTermToFormula a

             freshVar ← newTVar

             return $ if isCNameFOLConst folExists
                      then Exists freshVar $ const fm
                      else ForAll freshVar $ const fm

         | otherwise → predicate qName args

       (a1 : a2 : [])
         | isCNameFOLConstTwoHoles folAnd → binConst And a1 a2

         | isCNameFOLConstTwoHoles folOr → binConst Or a1 a2

         | isCNameFOLConstTwoHoles folImplies → binConst Implies a1 a2

         | isCNameFOLConstTwoHoles folEquiv → binConst Equiv a1 a2

         | isCNameFOLConstTwoHoles folEquals → do
             reportSLn "t2f" 20 "Processing equals"
             liftM2 equal (argTermToFOLTerm a1) (argTermToFOLTerm a2)

         | otherwise → predicate qName args

       _ → predicate qName args

       where
       isCNameFOLConst ∷ String → Bool
       isCNameFOLConst constFOL =
         -- The equality on the data type @C.Name@ is defined to ignore
         -- ranges, so we use @noRange@.
         cName == C.Name noRange [C.Id constFOL]

       isCNameFOLConstHoleRight ∷ String → Bool
       isCNameFOLConstHoleRight constFOL =
         -- The operators are represented by a list with @Hole@'s.  See
         -- the documentation for @C.Name@.
         cName == C.Name noRange [C.Id constFOL, C.Hole]

       isCNameFOLConstTwoHoles ∷ String → Bool
       isCNameFOLConstTwoHoles constFOL =
         -- The operators are represented by a list with @Hole@'s.  See
         -- the documentation for @C.Name@.
         cName == C.Name noRange [C.Hole, C.Id constFOL, C.Hole]

termToFormula term@(Lam _ (Abs _ termLam)) = do
  reportSLn "t2f" 10 $ "termToFormula Lam:\n" ++ show term

  _ ← pushTNewVar
  f ← termToFormula termLam
  popTVar

  return f

termToFormula (Pi domTy (NoAbs x absTy)) = do
  reportSLn "t2f" 10 $
    "termToFormula Pi _ (NoAbs _ _):\n"
    ++ "domTy: " ++ show domTy ++ "\n"
    ++ "absTy: " ++ show (NoAbs x absTy)
  f2 ← typeToFormula absTy

  if x /= "_"
    then
      case unDom domTy of
        -- The variable @x@ is an universal quantified variable not
        -- used, thefefore we generate a quantified first-order logic
        -- formula.
        El (Type (Max [])) (Def _ []) → do
          freshVar ← newTVar
          return $ ForAll freshVar $ const f2

        -- The variable @x@ is a proof term, therefore we erase the
        -- quantification on it.
        El (Type (Max [])) (Def _ _) → do
          f1 ← domTypeToFormula domTy
          return $ Implies f1 f2

        undom → do
          reportSLn "t2f" 20 $ "undom :" ++ show undom
          __IMPOSSIBLE__

    -- The variable @x@ is a proof term, therefore we erase the
    -- quantication on it.
    else do
      f1 ← domTypeToFormula domTy
      return $ Implies f1 f2

termToFormula (Pi domTy (Abs x absTy)) = do
  reportSLn "t2f" 10 $
    "termToFormula Pi _ (Abs _ _):\n"
    ++ "domTy: " ++ show domTy ++ "\n"
    ++ "absTy: " ++ show (Abs x absTy)

  freshVar ← pushTNewVar

  reportSLn "t2f" 20 $
    "Starting processing in local environment with fresh variable "
    ++ show freshVar ++ " and type:\n" ++ show absTy

  f ← typeToFormula absTy
  popTVar

  reportSLn "t2f" 20 $
    "Finalized processing in local environment with fresh variable "
    ++ show freshVar ++ " and type:\n" ++ show absTy

  reportSLn "t2f" 20 $ "The formula f is: " ++ show f

  case unDom domTy of
    -- The bounded variable is quantified on a @Set@,
    --
    -- e.g. the bounded variable is @d : D@ where @D : Set@,
    --
    -- so we can create a fresh variable and quantify on it without
    -- any problem.
    --
    -- N.B. the pattern matching on @(Def _ [])@.
    El (Type (Max [])) (Def _ []) → do
      reportSLn "t2f" 20 $
        "Adding universal quantification on variable " ++ show freshVar
      return $ ForAll freshVar $ const f

    -- The bounded variable is quantified on a proof. Due to we have
    -- drop the quantification on proofs terms, this case is
    -- impossible.
    El (Type (Max [])) (Def _ _) → __IMPOSSIBLE__

    -- Non-FOL translation: First-order logic universal quantified
    -- functions term.
    --
    -- The bounded variable is quantified on a function of a @Set@ to
    -- a @Set@,
    --
    -- e.g. the bounded variable is @f : D → D@, where @D : Set@.
    --
    -- In this case we handle the bounded variable/function as a FOL
    -- variable in @termToFOLTerm (Var n args)@, which is processed
    -- first due to lazyness. We quantified on this variable.
    El (Type (Max []))
       (Pi (Dom _ (El (Type (Max [])) (Def _ [])))
           (NoAbs _ (El (Type (Max [])) (Def _ [])))) → do
      reportSLn "t2f" 20
        "Removing a quantification on a function of a Set to a Set"
      return $ ForAll freshVar $ const f

    -- N.B. The next case is just a generalization to various
    -- arguments of the previous case.

    -- Non-FOL translation: First-order logic universal quantified
    -- functions term.
    --
    -- The bounded variable is quantified on a function of a @Set@ to
    -- a @Set@,
    --
    -- e.g. the bounded variable is @f : D → D → D@, where @D : Set@.
    --
    -- In this case we handle the bounded variable/function as a FOL
    -- variable in @termToFOLTerm (Var n args)@, which is processed
    -- first due to lazyness. We quantified on this variable.
    El (Type (Max []))
       (Pi (Dom _ (El (Type (Max [])) (Def _ [])))
           (NoAbs _ (El (Type (Max [])) (Pi _ (NoAbs _ _))))) → do
      reportSLn "t2f" 20
        "Removing a quantification on a function of a Set to a Set"
      -- 31 May 2012. We don't have an example of this case.
      --
      -- return $ ForAll freshVar (\_ → f)
      __IMPOSSIBLE__

    El (Type (Max [])) someTerm → do
      reportSLn "t2f" 20 $ "The term someterm is: " ++ show someTerm
      __IMPOSSIBLE__

    -- Non-FOL translation: First-order logic universal quantified
    -- formulae.
    --
    -- The bounded variable is quantified on a @Set₁@,
    --
    -- e.g. the bounded variable is @P : Set@,
    --
    -- so we just return the consequent. We use this case for translate
    -- predicate logical schemata such
    --
    -- @∨-comm  : {P Q : Set} → P ∨ Q → Q ∨ P@.
    --
    -- In this case we handle the bounded variable/function in
    -- @termToFormula (Var n args)@, which is processed first due to
    -- lazyness.

    El (Type (Max [ClosedLevel 1])) (Sort _) → do
      reportSLn "t2f" 20 $ "The type domTy is: " ++ show domTy

      let p ∷ String
          p = "--universal-quantified-formulae"

      ifM (isTPragmaOption p)
          (return f)
          (throwError $ universalQuantificationMsg p)

    -- Non-FOL translation: First-order logic universal quantified
    -- propositional functions.
    --
    -- The bounded variable is quantified on a @Set₁@,
    --
    -- e.g. the bounded variable is @P : D → D → Set@.
    --
    -- In this case we return a forall bind on the fresh variable. We
    -- use this case for translate predicate logic schemas, e.g.
    --
    --   ∨-comm₂ : {P₂ Q₂ : D → D → Set}{x y : D} →
    --             P₂ x y ∨ Q₂ x y → Q₂ x y ∨ P₂ x y

    El (Type (Max [ClosedLevel 1])) (Pi _ (NoAbs _ _)) → do
      reportSLn "t2f" 20 $ "The type domTy is: " ++ show domTy
      return $ ForAll freshVar $ const f

    someType → do
      reportSLn "t2f" 20 $ "The type domTy is: " ++ show someType
      __IMPOSSIBLE__

termToFormula term@(Var n args) = do
  reportSLn "t2f" 10 $ "termToFormula Var: " ++ show term

  when (n < 0) (__IMPOSSIBLE__)
  vars ← getTVars
  when (length vars <= n) (__IMPOSSIBLE__)

  case args of
    -- N.B. In this case we *don't* use Koen's approach.
    [] → return $ Predicate (vars !! n) []

    -- Non-FOL translation: First-order logic universal quantified
    -- propositional functions.

    -- If we have a bounded variable quantified on a function of a
    -- @Set@ to a @Set₁@, for example, the variable/predicate @P@ in
    --
    -- @(P : D → Set) → (x : D) → P x → P x@
    --
    -- we are quantifying on this variable/function
    --
    -- (see @termToFormula (Pi domTy (Abs _ absTy))@),
    --
    -- therefore we need to apply this variable/predicate to the
    -- others variables. See an example in
    -- Test.Succeed.AgdaInternalTerms.Var1.agda.
    _ → do
      let p ∷ String
          p = "--universal-quantified-propositional-functions"

      ifM (isTPragmaOption p)
          (ifM (askTOpt optWithoutPSymbols)
               (throwError $
                 "The options '--universal-quantified-propositional-functions'"
                 ++ " and '--without-predicate-symbols' are incompatible")
               (predicateLogicalScheme vars n args)
          )
          (throwError $ universalQuantificationMsg p)

termToFormula _ = __IMPOSSIBLE__

-- Translate the function @foo x1 ... xn@.
appArgsF ∷ String → Args → T FOLTerm
appArgsF fn args = do
  termsFOL ← mapM argTermToFOLTerm args
  ifM (askTOpt optWithFnApp)
      -- Trannslation using a binary function symbol.
      (return $ foldl' app (FOLFun fn []) termsFOL)
      -- Direct translation.
      (return $ FOLFun fn termsFOL)

-- | Translate an Agda internal 'Term' to a first-order logic term
-- 'FOLTerm'.
termToFOLTerm ∷ Term → T FOLTerm

termToFOLTerm term@(Con (QName _ name) args) = do
  reportSLn "t2t" 10 $ "termToFOLTerm Con:\n" ++ show term

  let cName ∷ C.Name
      cName = nameConcrete name

  case cName of
    C.NoName{}  → __IMPOSSIBLE__

    C.Name _ [] → __IMPOSSIBLE__

    -- The term @Con@ doesn't have holes. It should be translated as a
    -- first-order logic function.
    C.Name _ [C.Id str] →
     case args of
       [] → return $ FOLFun str []
       _  → appArgsF str args

    -- The term @Con@ has holes. It is translated as a first-order
    -- logic function.
    C.Name _ _ → __IMPOSSIBLE__
    -- 2012-04-22: We do not have an example of it.
    -- C.Name _ parts →
    --   case args of
    --     [] → __IMPOSSIBLE__
    --     _  → appArgsFn (concatName parts) args

termToFOLTerm term@(Def (QName _ name) args) = do
  reportSLn "t2t" 10 $ "termToFOLTerm Def:\n" ++ show term

  let cName ∷ C.Name
      cName = nameConcrete name

  case cName of
    C.NoName{}  → __IMPOSSIBLE__

    C.Name _ [] → __IMPOSSIBLE__

    -- The term @Def@ doesn't have holes. It is translated as a
    -- first-order logic function.
    C.Name _ [C.Id str] →
     case args of
       [] → return $ FOLFun str []
       _  → appArgsF str args

    -- The term @Def@ has holes. It is translated as a first-order
    -- logic function.
    C.Name _ parts →
      case args of
        [] → __IMPOSSIBLE__
        _  → appArgsF (concatName parts) args

termToFOLTerm term@(Lam (ArgInfo {argInfoHiding = NotHidden}) (Abs _ termLam)) = do
  reportSLn "t2f" 10 $ "termToFOLTerm Lam:\n" ++ show term

  _ ← pushTNewVar
  f ← termToFOLTerm termLam
  popTVar

  return f

termToFOLTerm term@(Var n args) = do
  reportSLn "t2t" 10 $ "termToFOLTerm Var:\n" ++ show term

  when (n < 0) (__IMPOSSIBLE__)
  vars ← getTVars
  when (length vars <= n) (__IMPOSSIBLE__)

  case args of
    [] → return $ FOLVar (vars !! n)

    -- Non-FOL translation: First-order logic universal quantified
    -- functions term.

    -- If we have a bounded variable quantified on a function of a Set
    -- to a Set, for example, the variable/function @f@ in
    --
    -- @(f : D → D) → (a : D) → (lam f) ∙ a ≡ f a@
    --
    -- we are quantifying on this variable/function
    --
    -- (see @termToFormula (Pi domTy (Abs _ absTy))@),
    --
    -- therefore we need to apply this variable/function to the others
    -- variables. See an example in
    -- Test.Succeed.AgdaInternalTerms.Var2.agda
    _varArgs → do
      let p ∷ String
          p = "--universal-quantified-functions"

      ifM (isTPragmaOption p)
          -- TODO (24 March 2013). Implementation.
          (throwError "The option '--universal-quantified-functions' is not implemented")
          -- (do termsFOL ← mapM argTermToFOLTerm varArgs
          --     ifM (askTOpt optAppF)
          --         (return $ foldl' app (FOLVar (vars !! n)) termsFOL)
          --         (return $ FOLFun (vars !! n) termsFOL))
          (throwError $ universalQuantificationMsg p)

termToFOLTerm _ = __IMPOSSIBLE__

------------------------------------------------------------------------------
-- Note [Non-dependent functions]

-- 27 June 2012. After the patch
--
-- Wed Sep 21 04:50:43 COT 2011  ulfn@chalmers.se
--   * got rid of the Fun constructor in internal syntax (using Pi _ (NoAbs _ _) instead)
--
-- Agda is using (Pi _ (NoAbs _ _)) for the non-dependent
-- functions. In a later patch, Agda changed somes (Pi _ (Abs _ _))
-- to (Pi _ (NoAbs _ _)). The solution below works for *all* our cases.

------------------------------------------------------------------------------
-- Note [Unique name]

-- Because the ATP pragma definitions are global, we need an unique
-- name. In this case, we append to the @qName@ the @qName@'s id (it
-- generates long TPTP name for the definitions).
