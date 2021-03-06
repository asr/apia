
-- | Translation of Agda internal functions to the target logic.

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UnicodeSyntax        #-}

-- Only are translated the functions that will be translate as TPTP
-- definitions.

module Apia.Translation.Functions ( fnToFormula ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Abstract.Name ( QName )

import Agda.Syntax.Common
  ( Arg(Arg)
  , defaultArgInfo
  , Dom(Dom)
  , Nat
  )

import Agda.Syntax.Internal
  ( Abs(Abs)
  , Clause(Clause)
  , Elim'(Apply)
  , Elims
  , Level(Max)
  , namedClausePats
  , PlusLevel(ClosedLevel)
  , Sort(Type)
  , Tele(ExtendTel)
  , Term(Def, Pi, Sort)
  , Type
  , Type'(El)
  , var
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.FOL.Types ( LFormula(Bicond, Eq, ForAll) )

import Apia.Monad.Base
  ( getTVars
  , popTVar
  , pushTNewVar
  , T
  , tErr
  , TErr ( NoFOLDefinition
         , NoOneClauseDefinition
         , ProofTermInDefintion
         )
  )

import Apia.Monad.Reports ( reportSLn )

import Apia.Translation.Terms
  ( agdaTermToFormula
  , agdaTermToTerm
  )

import Apia.Utils.AgdaAPI.EtaExpansion ( EtaExpandible(etaExpand) )

-- import Apia.Utils.AgdaAPI.IgnoreSharing ( IgnoreSharing(ignoreSharing) )

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
fnToFormula qName  ty [cl] =
  clauseToFormula qName ty cl (length $ namedClausePats cl)
fnToFormula qName _ _ = tErr $ NoOneClauseDefinition qName

-- A Clause is defined by (Agda.Syntax.Internal, 2016-12-25):

-- data Clause = Clause
--     { clauseRange     :: Range
--     , clauseTel       :: Telescope
--       -- ^ @Δ@: The types of the pattern variables in dependency order.
--     , namedClausePats :: [NamedArg DeBruijnPattern]
--       -- ^ @Δ ⊢ ps@.  The de Bruijn indices refer to @Δ@.
--     , clauseBody      :: Maybe Term
--       -- ^ @Just v@ with @Δ ⊢ v@ for a regular clause, or @Nothing@ for an
--       --   absurd one.
--     , clauseType      :: Maybe (Arg Type)
--       -- ^ @Δ ⊢ t@.  The type of the rhs under @clauseTel@.
--       --   Used, e.g., by @TermCheck@.
--       --   Can be 'Irrelevant' if we encountered an irrelevant projection
--       --   pattern on the lhs.
--     , clauseCatchall  :: Bool
--       -- ^ Clause has been labelled as CATCHALL.
--     }

-- The LHS of the definition's function is given by the @QName@ and
-- the @Type@. The RHS is given by the @Clause@. Before translating
-- the LHS and the RHS (i.e. the body of the clause) it is necessary
-- to generate a universal quantification on an equal number of
-- variables to length @[NamedArg DeBruijnPattern]@.
clauseToFormula ∷ QName → Type → Clause → Int → T LFormula
-- There is at most one variable in the clause's pattern.
clauseToFormula qName ty cl@(Clause lr fr tel (_ : ncps) (Just cBody) cTy cc unreachable)
                totalBoundedVars = do
  reportSLn "def2f" 20 $ "cl: " ++ show cl

  case tel of
    -- The bounded variable is quantified on a @Set@,
    --
    -- e.g. the bounded variable is @d : D@ where @D : Set@,
    --
    -- so we can create a fresh variable and quantify on it without
    -- any problem.
    --
    -- N.B. the pattern matching on @(Def _ [])@.
    ExtendTel (Dom _ _ (El (Type (Max [])) (Def _ []))) (Abs x tels) → do
      reportSLn "def2f" 20 $ "Processing variable " ++ show x

      freshVar ← pushTNewVar
      -- We process forward in the telescope and the pattern.
      f ← clauseToFormula qName ty (Clause lr fr tels ncps (Just cBody) cTy cc unreachable)
                          totalBoundedVars
      popTVar

      return $ ForAll freshVar $ const f

    -- The bounded variable is quantified on a proof,
    --
    -- e.g. the bounded variable is @Nn : N n@ where @D : Set@,
    -- @n : D@, and @N : D → Set@,
    --
    -- so we need drop this quantification. In this case, we erase the
    -- quantification on the bounded variable and we try it as a
    -- function type (using @Cond@ instead of @ForAll@).

    -- N.B. the pattern matching on @(Def _ _)@.

    -- See Issue #81.
    ExtendTel (Dom _ _ (El (Type (Max [])) (Def _ _))) (Abs _ _) →
      tErr $ ProofTermInDefintion qName

    ExtendTel (Dom _ _ (El (Type (Max [])) (Pi _ _))) _ →
      tErr $ NoFOLDefinition qName

    -- Issue #80.
    ExtendTel (Dom _ _ (El (Type (Max [_])) (Sort _))) _ →
      tErr $ NoFOLDefinition qName

    _ → do
        reportSLn "def2f" 20 $ "tel: " ++ show tel  -- (ignoreSharing tel)
        __IMPOSSIBLE__

-- The clause's patterns is empty, i.e. we have generated the required
-- universal quantification, so we translate the LHS and the RHS.
clauseToFormula qName ty (Clause _ _ _ [] (Just cBody) _ _ _) totalBoundedVars = do
  vars ← getTVars
  reportSLn "def2f" 20 $ "vars: " ++ show vars
  reportSLn "def2f" 20 $ "totalBoundedVars: " ++ show totalBoundedVars

  case ty of
    -- The defined symbol is a predicate.
    El (Type (Max [ClosedLevel 1])) _ → do

      -- We create the Agda term corresponds to the LHS of the symbol's
      -- definition.
      let lhs ∷ Term
          lhs = Def qName $ varsToElims $ length vars

      -- Because the LHS and the RHS (the body of the clause) are
      -- formulae, they are related via a biconditional connective.
      liftM2 Bicond
             (agdaTermToFormula lhs)
             (etaExpand cBody >>= agdaTermToFormula)

    -- The defined symbol is a function.
    El (Type (Max [])) _ → do
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

        -- 16 July 2012. N.B. We don't η-expand the term before the
        -- translation because we don't have a test case where it is
        -- neeed.
        then liftM2 Eq (agdaTermToTerm lhs) (agdaTermToTerm cBody)
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
            -- 16 July 2012. N.B. We don't η-expand the term before
            -- the translation (we don't have a test case where it is
            -- neeed).
            tRHS ← agdaTermToTerm cBody

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

clauseToFormula _ _ _ _  = __IMPOSSIBLE__
