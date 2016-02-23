------------------------------------------------------------------------------
-- |
-- Module      : Apia.Utils.Monad
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2015
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Monads utilities
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.Monad
  ( die
  , failureMsg
  , pair
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Apia.Utils.PrettyPrint ( Doc, prettyShow )

import System.Environment ( getProgName )
import System.Exit        ( exitFailure )

------------------------------------------------------------------------------
-- | Sequences a pair of monadic computations.
pair ∷ Monad m ⇒ m a → m b → m (a, b)
pair mx my = mx >>= \x → my >>= \y → return (x, y)

-- | Failure message.
failureMsg ∷ Doc → IO ()
failureMsg err =
  getProgName >>= \prg → hPutStrLn stderr $ prg ++ ": " ++ prettyShow err

-- | Exit with an error message.
die ∷ Doc → IO a
die err = failureMsg err >> exitFailure
