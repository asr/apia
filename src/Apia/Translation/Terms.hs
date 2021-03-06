
-- | Translation from Agda internal terms to the target logic.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Translation.Terms
  ( agdaTermToFormula
  , agdaTermToTerm
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name ( Name(nameConcrete) , QName(QName) )

import Agda.Syntax.Common
  ( Arg(Arg, argInfo, unArg)
  , ArgInfo(ArgInfo, argInfoHiding)
  , Dom(Dom, unDom)
  , Hiding(Hidden, NotHidden)
  , Nat
  )

import qualified Agda.Syntax.Concrete.Name as C
  ( Name(Name, NoName)
  , NamePart(Id, Hole)
  )

import Agda.Syntax.Internal as I
  ( Abs(Abs, NoAbs)
  , allApplyElims
  , Args
  , ConHead(ConHead)
  , Elim
  , Elim'(Apply, IApply, Proj)
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

import Apia.FOL.Constants
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

import Apia.FOL.Primitives ( appF, appP )

import Apia.FOL.Types as L
  ( LFormula( And
            , Bicond
            , Cond
            , Eq
            , Exists
            , FALSE
            , ForAll
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
  , tErr
  , TErr ( IncompatibleCLOptions
         , NoImplementedOption
         , UniversalQuantificationError
         )
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

import {-# source #-} Apia.Translation.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  )

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )
import Apia.Utils.AgdaAPI.Interface ( qNameToUniqueString )
import Apia.Utils.PrettyPrint       ( (<>), Pretty(pretty) )

#include "undefined.h"

------------------------------------------------------------------------------

agdaArgTermToFormula ∷ Arg Term → T LFormula
agdaArgTermToFormula Arg {argInfo = info, unArg = t} =
  case info of
    ArgInfo { argInfoHiding = NotHidden } → agdaTermToFormula t
    _                                     → __IMPOSSIBLE__

agdaArgTermToTerm ∷ Arg Term → T LTerm
agdaArgTermToTerm Arg {argInfo = info, unArg = t} =
  case info of
    ArgInfo { argInfoHiding = Hidden }    → agdaTermToTerm t
    ArgInfo { argInfoHiding = NotHidden } → agdaTermToTerm t
    _                                     → __IMPOSSIBLE__

binConst ∷ (LFormula → LFormula → LFormula) →
           Arg Term →
           Arg Term →
           T LFormula
binConst op arg1 arg2 =
  liftM2 op (agdaArgTermToFormula arg1) (agdaArgTermToFormula arg2)

elimToTerm ∷ Elim → T LTerm
elimToTerm (Apply arg) = agdaArgTermToTerm arg
elimToTerm (Proj _ _)  = __IMPOSSIBLE__
elimToTerm IApply{}    = __IMPOSSIBLE__

-- Translation of predicates.
predicate ∷ QName → Elims → T LFormula
predicate qName elims = do
  pName  ← qNameToUniqueString qName
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
agdaTermToFormula term'@(Def qName@(QName _ name) elims) = do
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

               | otherwise →
                 -- In this guard we translate 0-ary predicates, i.e.
                 -- propositional functions, for example, A : Set.
                 flip Predicate [] <$> qNameToUniqueString qName

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

         | isCNameTwoHoles lCond → binConst Cond a1 a2

         | isCNameTwoHoles lBicond1
           || isCNameTwoHoles lBicond2 → binConst Bicond a1 a2

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

agdaTermToFormula term'@(Lam _ (Abs _ termLam)) = do
  reportSLn "t2f" 10 $ "agdaTermToFormula Lam:\n" ++ show term'

  _ ← pushTNewVar
  f ← agdaTermToFormula termLam
  popTVar

  return f

agdaTermToFormula (Pi domTy (Abs x absTy)) = do
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
       (Pi (Dom _ _ (El (Type (Max [])) (Def _ [])))
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
       (Pi (Dom _ _ (El (Type (Max [])) (Def _ [])))
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
          (tErr $ UniversalQuantificationError p)

    someType → do
      reportSLn "t2f" 20 $ "The type domTy is: " ++ show someType
      __IMPOSSIBLE__

agdaTermToFormula (Pi domTy (NoAbs x absTy)) = do
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
          return $ Cond f1 f2

        -- The variable in @domTy@ has type @Set₁@
        -- (e.g. A : D → Set) and it isn't used, so we omit it.
        El (Type (Max [ClosedLevel 1])) (Pi _ (NoAbs _ _)) → return f2

        someType → do
          reportSLn "t2f" 20 $ "The type domTy is: " ++ show someType
          __IMPOSSIBLE__

    else do
      f1 ← agdaDomTypeToFormula domTy
      return $ Cond f1 f2

agdaTermToFormula term'@(I.Var n elims) = do
  reportSLn "t2f" 10 $ "agdaTermToFormula Var: " ++ show term'

  when (n < 0) (__IMPOSSIBLE__)
  vars ← getTVars

  -- We also test for equality because the first argument of @I.Var@
  -- doesn't start in one but in zero.
  when (length vars == n) (__IMPOSSIBLE__)
  when (length vars < n)  (__IMPOSSIBLE__)

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
               (tErr $ IncompatibleCLOptions
                  "--schematic-propositional-functions"
                  "--no-predicate-constants"
               )
               (propositionalFunctionScheme vars n elims)
          )
          (tErr $ UniversalQuantificationError p)

agdaTermToFormula term' = do
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
agdaTermToTerm term'@(Con (ConHead (QName _ name) _ _) _ elims) = do
  reportSLn "t2t" 10 $ "agdaTermToTerm Con:\n" ++ show term'

  let cName ∷ C.Name
      cName = nameConcrete name

  case cName of
    C.NoName{}  → __IMPOSSIBLE__

    C.Name _ [] → __IMPOSSIBLE__

    -- The term @Con@ doesn't have holes. It should be translated as
    -- a first-order logic function.
    C.Name _ [C.Id str] →
      case allApplyElims elims of
        Nothing    → __IMPOSSIBLE__
        Just []    → return $ Fun str []
        Just args  → appArgsF str args

    -- The term @Con@ has holes. It is translated as a first-order
    -- logic function.
    C.Name _ _ → __IMPOSSIBLE__
    -- 2012-04-22: We do not have an example of it.
    -- C.Name _ parts →
    --   case args of
    --     [] → __IMPOSSIBLE__
    --     _  → appArgsFn (concatName parts) args

agdaTermToTerm term'@(Def qName@(QName _ name) elims) = do
  reportSLn "t2t" 10 $ "agdaTermToTerm Def:\n" ++ show term'

  let cName ∷ C.Name
      cName = nameConcrete name

  case cName of
    C.NoName{}  → __IMPOSSIBLE__

    C.Name _ [] → __IMPOSSIBLE__

    -- The term @Def@ doesn't have holes. It is translated as a
    -- first-order logic function.
    C.Name _ [C.Id _] →
     case allApplyElims elims of
       Nothing    → __IMPOSSIBLE__
       Just []    → flip Fun [] <$> qNameToUniqueString qName
       Just args  → qNameToUniqueString qName >>= flip appArgsF args

    -- The term @Def@ has holes. It is translated as a first-order
    -- logic function.
    C.Name _ _ →
      case allApplyElims elims of
        Nothing    → __IMPOSSIBLE__
        Just []    → __IMPOSSIBLE__
        Just args  → qNameToUniqueString qName >>= flip appArgsF args

agdaTermToTerm term'@(Lam ArgInfo{argInfoHiding = NotHidden} (Abs _ termLam)) = do
  reportSLn "t2f" 10 $ "agdaTermToTerm Lam:\n" ++ show term'

  _ ← pushTNewVar
  f ← agdaTermToTerm termLam
  popTVar

  return f

agdaTermToTerm term'@(I.Var n args) = do
  reportSLn "t2t" 10 $ "agdaTermToTerm Var:\n" ++ show term'

  when (n < 0) (__IMPOSSIBLE__)
  vars ← getTVars

  -- We also test for equality because the first argument of @I.Var@
  -- doesn't start in one but in zero.
  when (length vars == n) (__IMPOSSIBLE__)
  when (length vars < n)  (__IMPOSSIBLE__)

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
          (tErr $ NoImplementedOption "--schematic-functions")
          -- (do lTerms ← mapM agdaArgTermToTerm varArgs
          --     ifM (askTOpt optAppF)
          --         (return $ foldl' app (Var (vars !! n)) lTerms)
          --         (return $ Fun (vars !! n) lTerms))
          (tErr $ UniversalQuantificationError p)

agdaTermToTerm _ = __IMPOSSIBLE__

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
