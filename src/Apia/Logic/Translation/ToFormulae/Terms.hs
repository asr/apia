------------------------------------------------------------------------------
-- |
-- Module      : Apia.Logic.Translation.ToFormulae.Internal.Terms
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation from Agda internal terms to target logic formulae.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Logic.Translation.ToFormulae.Terms
  ( agdaTermToFormula
  , agdaTermToTerm
  ) where

------------------------------------------------------------------------------

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
  , allApplyElims
  , Arg
  , Args
  , ConHead(ConHead)
  , Elim
  , Elim'(Apply, Proj)
  , Elims
  , Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Term(Con, Def, Lam, Pi, Sort, Var)
  , Type'(El)
  )

import Agda.Syntax.Position  ( noRange )
import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )
import Agda.Utils.Monad      ( ifM )

import Apia.Logic.Constants
  ( lTrue
  , lFalse
  , lNot
  , lAnd
  , lOr
  , lCond
  , lBicond1
  , lBicond2
  , lExists
  , lForAll
  , lEquals
  )

import Apia.Logic.Primitives ( appF, appP )

import {-# source #-} Apia.Logic.Translation.ToFormulae.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  )

import Apia.Logic.Types as L
  ( LFormula( And
            , Eq
            , Equiv
            , Exists
            , FALSE
            , ForAll
            , Implies
            , Not
            , Or
            , Predicate
            , TRUE
            )
  , LTerm(Fun, Var)
  )

import Apia.Monad.Base
  ( askTOpt
  , getTVars
  , newTVar
  , popTVar
  , pushTNewVar
  , T
  )

import Apia.Monad.Reports ( reportDLn, reportSLn )

import Apia.Options
  ( Options ( optFnConstant
            , optNoInternalEquality
            , optNoPredicateConstants
            , optSchematicFunctions
            , optSchematicPropositionalFunctions
            , optSchematicPropositionalSymbols
            )
  )

import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )
import Apia.Utils.AgdaAPI.Interface     ( isATPDefinition, qNameDefinition )

import qualified Apia.Utils.Except as E

import Apia.Utils.Name        ( concatName )
import Apia.Utils.PrettyPrint ( (<>), Doc, Pretty(pretty), squotes )

import Control.Monad ( liftM2, when )
import Data.List     ( foldl' )

#include "undefined.h"

------------------------------------------------------------------------------

universalQuantificationErrorMsg ∷ String → Doc
universalQuantificationErrorMsg p =
  pretty "use the " <> squotes p
  <> pretty " option for the translation of first-order logic universal quantified "
  <> pretty entities
  where
    entities ∷ String
    entities =
      case p of
        "--schematic-functions"               → "functions"
        "--schematic-propositional-symbols"   → "propositional symbols"
        "--schematic-propositional-functions" → "propositional functions"
        _                                     → __IMPOSSIBLE__

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
        NameId x i → return $ (show . nameConcrete) name ++ "_"
                              ++ show x ++ "_"
                              ++ show i
    else return $ show $ nameConcrete name

agdaArgTermToFormula ∷ I.Arg Term → T LFormula
agdaArgTermToFormula Arg {argInfo = info, unArg = t} =
  case info of
    ArgInfo { argInfoHiding = NotHidden } → agdaTermToFormula t
    _                                     → __IMPOSSIBLE__

agdaArgTermToTerm ∷ I.Arg Term → T LTerm
agdaArgTermToTerm Arg {argInfo = info, unArg = t} =
  case info of
    ArgInfo { argInfoHiding = Hidden }    → agdaTermToTerm t
    ArgInfo { argInfoHiding = NotHidden } → agdaTermToTerm t
    _                                     → __IMPOSSIBLE__

binConst ∷ (LFormula → LFormula → LFormula) →
           I.Arg Term →
           I.Arg Term →
           T LFormula
binConst op arg1 arg2 =
  liftM2 op (agdaArgTermToFormula arg1) (agdaArgTermToFormula arg2)

elimToTerm ∷ Elim → T LTerm
elimToTerm (Apply arg) = agdaArgTermToTerm arg
elimToTerm (Proj _)    = __IMPOSSIBLE__

-- Translation of predicates.
predicate ∷ QName → Elims → T LFormula
predicate qName elims = do
  pName  ← qName2String qName
  lTerms ← mapM elimToTerm elims

  case length elims of
    0 → __IMPOSSIBLE__
    _ → ifM (askTOpt optNoPredicateConstants)
            -- Direct translation.
            (return $ Predicate pName lTerms)
            -- Translation using Koen's suggestion.
            (return $ appP (Fun pName []) lTerms)

propositionalFunctionScheme ∷ [String] → Nat → Elims → T LFormula
propositionalFunctionScheme vars n elims = do
  let var ∷ String
      var = vars !! n

  case length elims of
    0 → __IMPOSSIBLE__
    _ → fmap (appP (L.Var var)) (mapM elimToTerm elims)

-- | Translate an Agda internal 'Term' to a target logic formula.
agdaTermToFormula ∷ Term → T LFormula
agdaTermToFormula term = case ignoreSharing term of

  term'@(Def qName@(QName _ name) elims) → do
    reportSLn "t2f" 10 $ "agdaTermToFormula Def:\n" ++ show term'

    let cName ∷ C.Name
        cName = nameConcrete name

    case cName of
      C.NoName{} → __IMPOSSIBLE__

      C.Name _ [] → __IMPOSSIBLE__

      C.Name{} →
       case allApplyElims elims of
         Nothing → __IMPOSSIBLE__

         Just [] | isCNameLogicConst lTrue  → return TRUE

                 | isCNameLogicConst lFalse → return FALSE

                 | otherwise → do
                   -- In this guard we translate 0-ary predicates, i.e.
                   -- propositional functions, for example, A : Set.
                   lName ← qName2String qName
                   return $ Predicate lName []

         Just [a]
           | isCNameHoleRight lNot → fmap Not (agdaArgTermToFormula a)

           | isCNameLogicConst lExists ||
             isCNameLogicConst lForAll → do
               fm ← agdaArgTermToFormula a

               freshVar ← newTVar

               return $ if isCNameLogicConst lExists
                        then Exists freshVar $ const fm
                        else ForAll freshVar $ const fm

           | otherwise → predicate qName elims

         Just [a1, a2]
           | isCNameTwoHoles lAnd → binConst And a1 a2

           | isCNameTwoHoles lOr → binConst Or a1 a2

           | isCNameTwoHoles lCond → binConst Implies a1 a2

           | isCNameTwoHoles lBicond1
             || isCNameTwoHoles lBicond2 → binConst Equiv a1 a2

           | isCNameTwoHoles lEquals → do
               reportSLn "t2f" 20 "Processing equals"
               ifM (askTOpt optNoInternalEquality)
                   -- Not using the ATPs internal equality.
                   (predicate qName elims)
                   -- Using the ATPs internal equality.
                   (liftM2 Eq (agdaArgTermToTerm a1) (agdaArgTermToTerm a2))

           | otherwise → predicate qName elims

         _ → predicate qName elims

         where
         isCNameLogicConst ∷ String → Bool
         isCNameLogicConst lConst =
           -- The equality on the data type @C.Name@ is defined to
           -- ignore ranges, so we use @noRange@.
           cName == C.Name noRange [C.Id lConst]

         isCNameHoleRight ∷ String → Bool
         isCNameHoleRight lConst =
           -- The operators are represented by a list with @Hole@'s.
           -- See the documentation for @C.Name@.
           cName == C.Name noRange [C.Id lConst, C.Hole]

         isCNameTwoHoles ∷ String → Bool
         isCNameTwoHoles lConst =
           -- The operators are represented by a list with @Hole@'s.  See
           -- the documentation for @C.Name@.
           cName == C.Name noRange [C.Hole, C.Id lConst, C.Hole]

  term'@(Lam _ (Abs _ termLam)) → do
    reportSLn "t2f" 10 $ "agdaTermToFormula Lam:\n" ++ show term'

    _ ← pushTNewVar
    f ← agdaTermToFormula termLam
    popTVar

    return f

  Pi domTy (Abs x absTy) → do
    reportSLn "t2f" 10 $
      "agdaTermToFormula Pi _ (Abs _ _):\n"
      ++ "domTy: " ++ show domTy ++ "\n"
      ++ "absTy: " ++ show (Abs x absTy)

    freshVar ← pushTNewVar

    reportSLn "t2f" 20 $
      "Starting processing in local environment with fresh variable "
      ++ show freshVar ++ " and type:\n" ++ show absTy

    f ← agdaTypeToFormula absTy
    popTVar

    reportSLn "t2f" 20 $
      "Finalized processing in local environment with fresh variable "
      ++ show freshVar ++ " and type:\n" ++ show absTy

    reportDLn "t2f" 20 $ pretty "The formula f is: " <> pretty f

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
      -- The bounded variable is quantified on a function of a @Set@
      -- to a @Set@,
      --
      -- e.g. the bounded variable is @f : D → D@, where @D : Set@.
      --
      -- In this case we handle the bounded variable/function as a FOL
      -- variable in @agdaTermToTerm (Var n args)@, which is processed
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
      -- The bounded variable is quantified on a function of a @Set@
      -- to a @Set@,
      --
      -- e.g. the bounded variable is @f : D → D → D@, where
      -- @D : Set@.
      --
      -- In this case we handle the bounded variable/function as a FOL
      -- variable in @agdaTermToTerm (Var n args)@, which is processed
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
      -- propositional functions.
      --
      -- The bounded variable is quantified on a @Set₁@,
      --
      -- e.g. the bounded variable is @A : D → D → Set@.
      --
      -- In this case we return a forall bind on the fresh variable. We
      -- use this case for translate logic schemata such as
      --
      --   ∨-comm₂ : {A₂ B₂ : D → D → Set}{x y : D} →
      --             A₂ x y ∨ B₂ x y → A₂ x y ∨ B₂ x y.

      El (Type (Max [ClosedLevel 1])) (Pi _ (NoAbs _ _)) → do
        reportSLn "t2f" 20 $ "The type domTy is: " ++ show domTy
        return $ ForAll freshVar $ const f

      -- Non-FOL translation: First-order logic universal quantified
      -- propositional symbols.
      --
      -- The bounded variable is quantified on a @Set₁@,
      --
      -- e.g. the bounded variable is @A : Set@,
      --
      -- so we just return the consequent. We use this case for
      -- translating logical schemata such
      --
      -- @∨-comm  : {A B : Set} → A ∨ B → B ∨ A@.
      --
      -- In this case we handle the bounded variable/function in
      -- @agdaTermToFormula (Var n args)@, which is processed first due to
      -- lazyness.

      El (Type (Max [ClosedLevel 1])) (Sort _) → do
        reportSLn "t2f" 20 $ "The type domTy is: " ++ show domTy

        let p ∷ String
            p = "--schematic-propositional-symbols"

        ifM (askTOpt optSchematicPropositionalSymbols)
            (return f)
            (E.throwE $ universalQuantificationErrorMsg p)

      someType → do
        reportSLn "t2f" 20 $ "The type domTy is: " ++ show someType
        __IMPOSSIBLE__

  Pi domTy (NoAbs x absTy) → do
    reportSLn "t2f" 10 $
      "agdaTermToFormula Pi _ (NoAbs _ _):\n"
      ++ "domTy: " ++ show domTy ++ "\n"
      ++ "absTy: " ++ show (NoAbs x absTy)
    f2 ← agdaTypeToFormula absTy

    if x /= "_"
      then
        case unDom domTy of
          -- The variable @x@ is an universal quantified variable not
          -- used, thefefore we generate a quantified first-order
          -- logic formula.
          El (Type (Max [])) (Def _ []) → do
            freshVar ← newTVar
            return $ ForAll freshVar $ const f2

          -- The variable @x@ is a proof term, therefore we erase the
          -- quantification on it.
          El (Type (Max [])) (Def _ _) → do
            f1 ← agdaDomTypeToFormula domTy
            return $ Implies f1 f2

          -- The variable in @domTy@ has type @Set₁@
          -- (e.g. A : D → Set) and it isn't used, so we omit it.
          El (Type (Max [ClosedLevel 1])) (Pi _ (NoAbs _ _)) → return f2

          someType → do
            reportSLn "t2f" 20 $ "The type domTy is: " ++ show someType
            __IMPOSSIBLE__

      else do
        f1 ← agdaDomTypeToFormula domTy
        return $ Implies f1 f2

  term'@(I.Var n elims) → do
    reportSLn "t2f" 10 $ "agdaTermToFormula Var: " ++ show term'

    when (n < 0) (__IMPOSSIBLE__)
    vars ← getTVars
    when (length vars <= n) (__IMPOSSIBLE__)

    case elims of
      -- N.B. In this case we *don't* use Koen's approach.
      [] → return $ Predicate (vars !! n) []

      -- Non-FOL translation: First-order logic universal quantified
      -- propositional functions.

      -- If we have a bounded variable quantified on a function of a
      -- @Set@ to a @Set₁@, for example, the variable/function @A@ in
      --
      -- @(A : D → Set) → (x : D) → A x → A x@
      --
      -- we are quantifying on this variable/function
      --
      -- (see @agdaTermToFormula (Pi domTy (Abs _ absTy))@),
      --
      -- therefore we need to apply this variable/function to the
      -- others variables.
      _ → do
        let p ∷ String
            p = "--schematic-propositional-functions"

        ifM (askTOpt optSchematicPropositionalFunctions)
            (ifM (askTOpt optNoPredicateConstants)
                 (E.throwE $
                   pretty "the " <> squotes "--schematic-propositional-functions"
                   <> pretty " and "
                   <> squotes "--no-predicate-constants"
                   <> pretty " options are incompatible")
                 (propositionalFunctionScheme vars n elims)
            )
            (E.throwE $ universalQuantificationErrorMsg p)

  term' → do
    reportSLn "t2f" 20 $ "term: " ++ show term'
    __IMPOSSIBLE__

-- Translate the function @foo x1 ... xn@.
appArgsF ∷ String → Args → T LTerm
appArgsF fn args = do
  lTerms ← mapM agdaArgTermToTerm args
  ifM (askTOpt optFnConstant)
      -- Translation using a hard-coded binary function symbol.
      (return $ foldl' appF (Fun fn []) lTerms)
      -- Direct translation.
      (return $ Fun fn lTerms)

-- | Translate an Agda internal 'Term' to a target logic term.
agdaTermToTerm ∷ Term → T LTerm
agdaTermToTerm term = case ignoreSharing term of

  term'@(Con (ConHead (QName _ name) _ _) args) → do
    reportSLn "t2t" 10 $ "agdaTermToTerm Con:\n" ++ show term'

    let cName ∷ C.Name
        cName = nameConcrete name

    case cName of
      C.NoName{}  → __IMPOSSIBLE__

      C.Name _ [] → __IMPOSSIBLE__

      -- The term @Con@ doesn't have holes. It should be translated as
      -- a first-order logic function.
      C.Name _ [C.Id str] →
       case args of
         [] → return $ Fun str []
         _  → appArgsF str args

      -- The term @Con@ has holes. It is translated as a first-order
      -- logic function.
      C.Name _ _ → __IMPOSSIBLE__
      -- 2012-04-22: We do not have an example of it.
      -- C.Name _ parts →
      --   case args of
      --     [] → __IMPOSSIBLE__
      --     _  → appArgsFn (concatName parts) args

  term'@(Def (QName _ name) elims) → do
    reportSLn "t2t" 10 $ "agdaTermToTerm Def:\n" ++ show term'

    let cName ∷ C.Name
        cName = nameConcrete name

    case cName of
      C.NoName{}  → __IMPOSSIBLE__

      C.Name _ [] → __IMPOSSIBLE__

      -- The term @Def@ doesn't have holes. It is translated as a
      -- first-order logic function.
      C.Name _ [C.Id str] →
       case allApplyElims elims of
         Nothing    → __IMPOSSIBLE__
         Just []    → return $ Fun str []
         Just args  → appArgsF str args

      -- The term @Def@ has holes. It is translated as a first-order
      -- logic function.
      C.Name _ parts →
        case allApplyElims elims of
          Nothing    → __IMPOSSIBLE__
          Just []    → __IMPOSSIBLE__
          Just args  → appArgsF (concatName parts) args

  term'@(Lam (ArgInfo {argInfoHiding = NotHidden}) (Abs _ termLam)) → do
    reportSLn "t2f" 10 $ "agdaTermToTerm Lam:\n" ++ show term'

    _ ← pushTNewVar
    f ← agdaTermToTerm termLam
    popTVar

    return f

  term'@(I.Var n args) → do
    reportSLn "t2t" 10 $ "agdaTermToTerm Var:\n" ++ show term'

    when (n < 0) (__IMPOSSIBLE__)
    vars ← getTVars
    when (length vars <= n) (__IMPOSSIBLE__)

    case args of
      [] → return $ L.Var (vars !! n)

      -- Non-FOL translation: First-order logic universal quantified
      -- functions term.

      -- If we have a bounded variable quantified on a function of a
      -- Set to a Set, for example, the variable/function @f@ in
      --
      -- @(f : D → D) → (a : D) → (lam f) ∙ a ≡ f a@
      --
      -- we are quantifying on this variable/function
      --
      -- (see @agdaTermToFormula (Pi domTy (Abs _ absTy))@),
      --
      -- therefore we need to apply this variable/function to the
      -- others variables. See an example in
      -- Test.Succeed.AgdaInternalTerms.Var2.agda
      _varArgs → do
        let p ∷ String
            p = "--schematic-functions"

        ifM (askTOpt optSchematicFunctions)
            -- TODO (24 March 2013). Implementation.
            (E.throwE $ pretty "the option " <> squotes "--schematic-functions"
                        <> pretty " is not implemented")
            -- (do lTerms ← mapM agdaArgTermToTerm varArgs
            --     ifM (askTOpt optAppF)
            --         (return $ foldl' app (Var (vars !! n)) lTerms)
            --         (return $ Fun (vars !! n) lTerms))
            (E.throwE $ universalQuantificationErrorMsg p)

  _ → __IMPOSSIBLE__

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
