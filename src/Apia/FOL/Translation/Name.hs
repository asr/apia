------------------------------------------------------------------------------
-- |
-- Module      : Apia.FOL.Translation.Concrete.Name
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Translation of things about Agda concrete names.
------------------------------------------------------------------------------

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Apia.FOL.Translation.Name ( concatName ) where

------------------------------------------------------------------------------
-- Agda library imports

import Agda.Syntax.Concrete.Name ( NamePart(Id, Hole) )

------------------------------------------------------------------------------

takeId ∷ NamePart → String
takeId Hole         = []
takeId (Id strName) = strName

-- | Use the parts of a name to produce a new function name, e.g. the
-- function @if_then_else_@ is called @ifthenelseq@.
concatName ∷ [NamePart] → String
concatName = concatMap takeId
