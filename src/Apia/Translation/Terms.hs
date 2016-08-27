
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

import Apia.Common ( Lang(SMT2, TPTP) )

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
            , optLang
            , optNoInternalEquality
            , optNoPredicateConstants
            , optSchematicFunctions
            , optSchematicPropositionalFunctions
            , optSchematicPropositionalSymbols
            )
  )

import Apia.TargetLang.Types ( TargetFormula(FOLFormula, SMT2Expr, SMT2Type) )

import {-# source #-} Apia.Translation.Types
  ( agdaDomTypeToFormula
  , agdaTypeToFormula
  )

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )
import Apia.Utils.AgdaAPI.Interface     ( qNameToUniqueString )

import qualified Apia.Utils.Except as E

import Apia.Utils.PrettyPrint ( (<>), Doc, Pretty(pretty), squotes )

import Apia.Utils.SMT2
  ( smt2And
  , smt2Bicond
  , smt2C
  , smt2Cond
  , smt2False
  , SMT2Expr
  , smt2Not
  , smt2Or
  , smt2True
  , tBool
  )

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

agdaArgTermToFormula ∷ Arg Term → T TargetFormula
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

tptpBinConst ∷ (LFormula → LFormula → LFormula) →
           Arg Term →
           Arg Term →
           T LFormula
tptpBinConst op arg1 arg2 = do
  lang ← askTOpt optLang
  f1   ← agdaArgTermToFormula arg1
  f2   ← agdaArgTermToFormula arg2

  case (lang, f1, f2) of
    (TPTP, FOLFormula _f1, FOLFormula _f2) → return $ op _f1 _f2
    _                                      → __IMPOSSIBLE__

-- TODO (2016-05-03): Refactor with `tptpBinConst`.
smt2BinConst ∷ (SMT2Expr → SMT2Expr → SMT2Expr) →
               Arg Term →
               Arg Term →
               T SMT2Expr
smt2BinConst op arg1 arg2 = do
  lang ← askTOpt optLang
  f1   ← agdaArgTermToFormula arg1
  f2   ← agdaArgTermToFormula arg2

  case (lang, f1, f2) of
    (SMT2, SMT2Expr _f1, SMT2Expr _f2) → return $ op _f1 _f2
    _                                  → __IMPOSSIBLE__

elimToTerm ∷ Elim → T LTerm
elimToTerm (Apply arg) = agdaArgTermToTerm arg
elimToTerm (Proj _)    = __IMPOSSIBLE__

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
agdaTermToFormula ∷ Term → T TargetFormula
agdaTermToFormula term'@(Def qName@(QName _ name) elims) = do
  reportSLn "t2f" 10 $ "agdaTermToFormula Def:\n" ++ show term'

  lang ← askTOpt optLang

  let cName ∷ C.Name
      cName = nameConcrete name

  case cName of
    C.NoName{} → __IMPOSSIBLE__

    C.Name _ [] → __IMPOSSIBLE__

    C.Name{} →
     case allApplyElims elims of
       Nothing → __IMPOSSIBLE__

       Just [] | isCNameLogicConst lTrue →
                   case lang of
                     TPTP → return $ FOLFormula TRUE
                     SMT2 → return $ SMT2Expr smt2True

               | isCNameLogicConst lFalse →
                   case lang of
                     TPTP → return $ FOLFormula FALSE
                     SMT2 → return $ SMT2Expr smt2False

               | otherwise → do
                   -- In this guard we translate 0-ary predicates,
                   -- i.e.  propositional functions, for example,
                   -- A : Set.
                   str ← qNameToUniqueString qName
                   case lang of
                     TPTP → return $ FOLFormula $ Predicate str []
                     SMT2 → return $ SMT2Expr $ smt2C . fromString $ str

       Just [a]
         | isCNameHoleRight lNot → do
             fm ← agdaArgTermToFormula a
             case (lang, fm) of
               (TPTP, FOLFormula _fm) → return $ FOLFormula $ Not _fm
               (SMT2, SMT2Expr _fm)   → return $ SMT2Expr $ smt2Not _fm
               _                      → __IMPOSSIBLE__

         | isCNameLogicConst lExists ||
           isCNameLogicConst lForAll → do
             fm ← agdaArgTermToFormula a

             freshVar ← newTVar

             case (lang, fm) of
               (TPTP, FOLFormula _fm) →
                 return $
                   if isCNameLogicConst lExists
                   then FOLFormula $ Exists freshVar $ const _fm
                   else FOLFormula $ ForAll freshVar $ const _fm
               _ → __IMPOSSIBLE__

         | otherwise → FOLFormula <$> predicate qName elims

       Just [a1, a2]
         | isCNameTwoHoles lAnd →
             case lang of
               TPTP → FOLFormula <$> tptpBinConst And a1 a2
               SMT2 → SMT2Expr <$> smt2BinConst smt2And a1 a2

         | isCNameTwoHoles lOr →
             case lang of
               TPTP → FOLFormula <$> tptpBinConst Or a1 a2
               SMT2 → SMT2Expr <$> smt2BinConst smt2Or a1 a2

         | isCNameTwoHoles lCond →
             case lang of
               TPTP → FOLFormula <$> tptpBinConst Implies a1 a2
               SMT2 → SMT2Expr <$> smt2BinConst smt2Cond a1 a2

         | isCNameTwoHoles lBicond1
           || isCNameTwoHoles lBicond2 →
             case lang of
               TPTP → FOLFormula <$> tptpBinConst Equiv a1 a2
               SMT2 → SMT2Expr <$> smt2BinConst smt2Bicond a1 a2

         | isCNameTwoHoles lEquals → do
             reportSLn "t2f" 20 "Processing equals"
             ifM (askTOpt optNoInternalEquality)
                 -- Not using the ATPs internal equality.
                 (FOLFormula <$> predicate qName elims)
                 -- Using the ATPs internal equality.
                 (FOLFormula <$> liftM2 Eq (agdaArgTermToTerm a1) (agdaArgTermToTerm a2))

         | otherwise → FOLFormula <$> predicate qName elims

       _ → FOLFormula <$> predicate qName elims

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

  lang     ← askTOpt optLang
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
      case (lang, f) of
        (TPTP, FOLFormula _f) →
          return $ FOLFormula $ ForAll freshVar $ const _f
        _ → __IMPOSSIBLE__

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
      case (lang, f) of
        (TPTP, FOLFormula _f) →
          return $ FOLFormula $ ForAll freshVar $ const _f
        _ → __IMPOSSIBLE__

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
      case (lang, f) of
        (TPTP, FOLFormula _f) →
          return $ FOLFormula $ ForAll freshVar $ const _f
        _ → __IMPOSSIBLE__

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

agdaTermToFormula (Pi domTy (NoAbs x absTy)) = do
  reportSLn "t2f" 10 $
    "agdaTermToFormula Pi _ (NoAbs _ _):\n"
    ++ "domTy: " ++ show domTy ++ "\n"
    ++ "absTy: " ++ show (NoAbs x absTy)

  lang ← askTOpt optLang
  f2   ← agdaTypeToFormula absTy

  if x /= "_"
    then
      case unDom domTy of
        -- The variable @x@ is an universal quantified variable not
        -- used, thefefore we generate a quantified first-order
        -- logic formula.
        El (Type (Max [])) (Def _ []) → do
          freshVar ← newTVar
          case (lang, f2) of
            (TPTP, FOLFormula _f2) →
              return $ FOLFormula $ ForAll freshVar $ const _f2
            _ → __IMPOSSIBLE__

        -- The variable @x@ is a proof term, therefore we erase the
        -- quantification on it.
        El (Type (Max [])) (Def _ _) → do
          f1 ← agdaDomTypeToFormula domTy
          case (lang, f1, f2) of
            (TPTP, FOLFormula _f1, FOLFormula _f2) →
              return $ FOLFormula $ Implies _f1 _f2
            _ → __IMPOSSIBLE__

        -- The variable in @domTy@ has type @Set₁@
        -- (e.g. A : D → Set) and it isn't used, so we omit it.
        El (Type (Max [ClosedLevel 1])) (Pi _ (NoAbs _ _)) → return f2

        someType → do
          reportSLn "t2f" 20 $ "The type domTy is: " ++ show someType
          __IMPOSSIBLE__

    else do
      f1 ← agdaDomTypeToFormula domTy
      case (lang, f1, f2) of
        (TPTP, FOLFormula _f1, FOLFormula _f2) →
          return $ FOLFormula $ Implies _f1 _f2

        (SMT2, SMT2Expr _f1, SMT2Expr _f2) →
          return $ SMT2Expr $ smt2Cond _f1 _f2

        _  → do
          reportDLn "t2f" 20 $
            pretty "f1 is: " <> pretty f1 <> pretty "\n"
            <> pretty "f2 is: " <> pretty f2
          __IMPOSSIBLE__

agdaTermToFormula term'@(Sort (Type (Max []))) = do
  reportSLn "t2f" 10 $ "agdaTermToFormula Var: " ++ show term'

  lang ← askTOpt optLang

  case lang of
    TPTP → __IMPOSSIBLE__
    SMT2 → return $ SMT2Type tBool

agdaTermToFormula term'@(I.Var n elims) = do
  reportSLn "t2f" 10 $ "agdaTermToFormula Var: " ++ show term'

  when (n < 0) (__IMPOSSIBLE__)
  vars ← getTVars
  when (length vars <= n) (__IMPOSSIBLE__)

  case elims of
    -- N.B. In this case we *don't* use Koen's approach.
    [] → return $ FOLFormula $ Predicate (vars !! n) []

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
               (FOLFormula <$> propositionalFunctionScheme vars n elims)
          )
          (E.throwE $ universalQuantificationErrorMsg p)

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
agdaTermToTerm term'@(Con (ConHead (QName _ name) _ _) args) = do
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
