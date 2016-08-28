
-- | Translation of Agda internal functions to the target logic.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- Only are translated the functions that will be translate as TPTP
-- definitions.

module Apia.Translation.Functions ( fnToFormula ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Common
  ( Arg(Arg)
  , defaultArgInfo
  , Dom(Dom)
  , Nat
  )

import Agda.Syntax.Abstract.Name ( QName )

import Agda.Syntax.Internal
  ( Abs(Abs)
  , Clause(Clause)
  , ClauseBody
  , Elim'(Apply)
  , Elims
  , Level(Max)
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Tele(ExtendTel)
  , Telescope
  , Term(Def, Pi)
  , Type
  , Type'(El)
  , var
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import qualified Agda.Utils.Pretty as AP

import Apia.FOL.Types ( LFormula(Bicond, Cond, Eq, ForAll) )

import Apia.Monad.Base
  ( getTVars
  , popTVar
  , pushTNewVar
  , T
  )

import Apia.Monad.Reports ( reportDLn, reportSLn )

import Apia.Translation.ClauseBody
  ( cBodyToFormula
  , cBodyToTerm
  , dropProofTermOnCBody
  )

import Apia.Translation.Terms
  ( agdaTermToFormula
  , agdaTermToTerm
  )

import Apia.Translation.Types ( agdaTypeToFormula )

import Apia.Utils.AgdaAPI.DeBruijn ( DecIndex(decIndex) )
import Apia.Utils.AgdaAPI.Vars     ( BoundedVars(boundedVars) )

import qualified Apia.Utils.Except as E

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )
import Apia.Utils.PrettyPrint           ( (<>), bquotes, Pretty(pretty) )

#include "undefined.h"

------------------------------------------------------------------------------
-- Auxiliary functions

varsToElims ∷ Nat → Elims
varsToElims 0 = []
varsToElims n = Apply (Arg defaultArgInfo (var (n - 1))) : varsToElims (n - 1)

------------------------------------------------------------------------------
-- In general a definition's function is given by various clauses
-- (i.e. equations), for example every equation in a definition by
-- pattern matching. In our case it is only necessary to translate
-- definitions with only one clause.

-- | Translate an ATP definition to a target logic formula.
fnToFormula ∷ QName → Type → [Clause] → T LFormula
fnToFormula _      _  []   = __IMPOSSIBLE__
fnToFormula qName  ty [cl] = clauseToFormula qName ty cl
fnToFormula qName  _  _    =
  E.throwE $ pretty "the translation of " <> bquotes (AP.pretty qName)
             <> pretty " failed because its definition only can have a clause"

-- A Clause is defined by (Agda.Syntax.Internal)
-- data Clause = Clause
--     { clauseRange     ∷ Range
--     , clauseTel       ∷ Telescope
--     , clausePerm      ∷ Permutation
--     , clausePats      ∷ [Arg Pattern]
--     , clauseBody      ∷ ClauseBody
--     }

-- The LHS of the definition's function is given by the @QName@ and
-- the @Type@. The RHS is given by the @Clause@. Before translate the
-- LHS and the RHS (i.e. the body of the clause) it is necessary to
-- generate an universal quantification on an equal number of
-- variables to length @[Arg Pattern]@.
clauseToFormula ∷ QName → Type → Clause → T LFormula

-- There is at most one variable in the clause's pattern.
clauseToFormula qName ty (Clause r tel (_ : pats) cBody cTy cc) =
  case tel of
    -- The bounded variable is quantified on a @Set@,
    --
    -- e.g. the bounded variable is @d : D@ where @D : Set@,
    --
    -- so we can create a fresh variable and quantify on it without any
    -- problem.
    --
    -- N.B. the pattern matching on @(Def _ [])@.
    ExtendTel (Dom _ (El (Type (Max [])) (Def _ []))) (Abs x tels) → do
      reportSLn "def2f" 20 $ "Processing variable " ++ show x

      freshVar ← pushTNewVar
      -- We process forward in the telescope and the pattern.
      f ← clauseToFormula qName ty (Clause r tels pats cBody cTy cc)
      popTVar

      return $ ForAll freshVar $ const f

    -- The bounded variable is quantified on a proof,
    --
    -- e.g. the bounded variable is @Nn : N n@ where @D : Set@, @n :
    -- D@, and @N : D → Set@,
    --
    -- so we need drop this quantification. In this case, we erase the
    -- quantification on the bounded variable and we try it as a
    -- function type (using @Cond@ instead of @ForAll@).

    -- N.B. the pattern matching on @(Def _ _)@.
    ExtendTel (Dom _ tye@(El (Type (Max [])) (Def _ _))) (Abs x tels) → do
      reportSLn "def2f" 20 $ "Processing proof term " ++ show x

      reportSLn "def2f" 20 $ "tye: " ++ show tye

      f1 ← agdaTypeToFormula tye

      reportDLn "def2f" 20 $ pretty "f1: " <> pretty f1

      reportSLn "def2f" 20 $ "Current body: " ++ show cBody

      newBody ∷ ClauseBody ← dropProofTermOnCBody cBody x

      -- Just to force the evaluation of @newBody@.
      when (null $ show newBody) (__IMPOSSIBLE__)

      reportSLn "def2f" 20 $ "New body: " ++ show newBody

      let decTels ∷ Telescope
          decTels = decIndex tels

      reportSLn "def2f" 20 $ "tels: " ++ show tels
      reportSLn "def2f" 20 $ "decTels: " ++ show decTels

      -- We process forward in the telescope and the pattern.
      f2 ← clauseToFormula qName ty (Clause r decTels pats newBody cTy cc)

      reportDLn "def2f" 20 $ pretty "f2: " <> pretty f2

      return $ Cond f1 f2

    ExtendTel (Dom _ (El (Type (Max [])) (Pi _ _))) _ →
      E.throwE $ pretty "the translation of " <> bquotes (AP.pretty qName)
                 <> pretty " failed because it is a higher-order definition"

    _ → do
        reportSLn "def2f" 20 $ "tel: " ++ show tel  -- (ignoreSharing tel)
        __IMPOSSIBLE__

-- The clause's patterns is empty, i.e. we have generated the required
-- universal quantification, so we translate the LHS and the RHS.
clauseToFormula qName ty (Clause _ _ [] cBody _ _) = do
  vars ← getTVars
  reportSLn "def2f" 20 $ "vars: " ++ show vars

  case ty of
    -- The defined symbol is a predicate.
    El (Type (Max [ClosedLevel 1])) _ → do

      -- We create the Agda term corresponds to the LHS of the symbol's
      -- definition.
      let lhs ∷ Term
          lhs = Def qName $ varsToElims $ length vars

      -- Because the LHS and the RHS (the body of the clause) are
      -- formulae, they are related via an biconditional connective,
      liftM2 Bicond (agdaTermToFormula lhs) (cBodyToFormula cBody)

    -- The defined symbol is a function.
    El (Type (Max [])) _ → do
      let totalBoundedVars ∷ Int
          totalBoundedVars = boundedVars cBody

      -- We create the Agda term corresponds to the LHS of the symbol's
      -- definition.

      -- We use @totalBoundedVars@ because we need to handle definitions like
      --
      -- @foo ∷ D → D@
      -- @foo = λ d → ...

      let lhs ∷ Term
          lhs = Def qName $ varsToElims totalBoundedVars

      if length vars == totalBoundedVars
        -- The definition is of the form
        --
        -- @foo ∷ D → D@
        -- @foo d = ...
        --
        -- so we don't need to add new fresh variables.
        then liftM2 Eq (agdaTermToTerm lhs) (cBodyToTerm cBody)
        -- The definition is of the form
        --
        -- @foo ∷ D → D@
        -- @foo = λ d → ...
        --
        -- so we need to add some fresh variables to the state before
        -- call the translation for @lhs@ and @cBody@.
        else if length vars < totalBoundedVars
          then do
            let diff ∷ Int
                diff = totalBoundedVars - length vars

            freshVars ← replicateM diff pushTNewVar
            reportSLn "def2f" 20 $ "Freshvars: " ++ show freshVars
            tLHS ← agdaTermToTerm lhs
            replicateM_ diff popTVar
            tRHS ← cBodyToTerm cBody

            -- Because the LHS and the RHS (the body of the clause)
            -- are terms, they are related via the target logic
            -- equaliy.
            let helper ∷ [String] → LFormula
                helper []       = __IMPOSSIBLE__
                helper [x]      = ForAll x $ \_ → Eq tLHS tRHS
                helper (x : xs) = ForAll x $ \_ → helper xs

            return $ helper freshVars
          else __IMPOSSIBLE__

    _ → __IMPOSSIBLE__
