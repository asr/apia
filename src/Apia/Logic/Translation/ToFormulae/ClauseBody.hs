------------------------------------------------------------------------------
-- |
-- Module      : Apia.Logic.Translation.ToFormulae.ClauseBody
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation of Agda internal @ClauseBody@ to the target logic
-- formulae and terms.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.Logic.Translation.ToFormulae.ClauseBody
  ( cBodyToFormula
  , cBodyToTerm
  , dropProofTermOnCBody
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Syntax.Common ( Nat )

import Agda.Syntax.Internal
  ( Abs(Abs)
  , ClauseBody
  , ClauseBodyF(Bind, Body)
  )

import Agda.Utils.Impossible ( Impossible(Impossible), throwImpossible )

import Apia.Logic.Translation.ToFormulae.Terms
  ( agdaTermToFormula
  , agdaTermToTerm
  )

import Apia.Logic.Types                ( LFormula, LTerm )
import Apia.Monad.Base                 ( T )
import Apia.Monad.Reports              ( reportSLn )
import Apia.Utils.AgdaAPI.DeBruijn     ( ChangeIndex(changeIndex), varToIndex )
import Apia.Utils.AgdaAPI.EtaExpansion ( EtaExpandible(etaExpand) )

#include "undefined.h"

------------------------------------------------------------------------------
-- | Translate an Agda internal 'ClauseBody' to a target logical
-- formula.
cBodyToFormula ∷ ClauseBody → T LFormula
cBodyToFormula (Body term)          = etaExpand term >>= agdaTermToFormula
cBodyToFormula (Bind (Abs _ cBody)) = cBodyToFormula cBody
cBodyToFormula _                    = __IMPOSSIBLE__

-- | Translate an Agda internal 'ClauseBody' to a logical term.
cBodyToTerm ∷ ClauseBody → T LTerm
-- 16 July 2012. N.B. We don't η-expand the term before the
-- translation (we don't have a test case where it is neeed).
cBodyToTerm (Body term)          = agdaTermToTerm term
cBodyToTerm (Bind (Abs _ cBody)) = cBodyToTerm cBody
cBodyToTerm _                    = __IMPOSSIBLE__

dropProofTermOnCBodyIndex ∷ ClauseBody → String → Nat → ClauseBody
dropProofTermOnCBodyIndex (Bind (Abs x1 cBody)) x2 index =
  if x1 == x2
  then changeIndex cBody index  -- We drop the bind and rename the
                                -- variables inside the body.
  else Bind (Abs x1 $ dropProofTermOnCBodyIndex cBody x2 index)
dropProofTermOnCBodyIndex _ _ _ = __IMPOSSIBLE__

-- To drop the binding on a proof term in a @ClauseBody@,
--
-- e.g. drop the binding on @Nn : N n@ where @D : Set@, @n : D@ and @N
-- : D → Set@.
--
-- We know that the bounded variable is a proof term from the
-- invocation to this function.

-- | Drop a proof term from an Agda internal 'ClauseBody'.
dropProofTermOnCBody ∷ ClauseBody → String → T ClauseBody
dropProofTermOnCBody cBody x = do
  let index ∷ Nat
      index = varToIndex cBody x

  reportSLn "drop" 20 $ "The index is: " ++ show index

  return $ dropProofTermOnCBodyIndex cBody x index
