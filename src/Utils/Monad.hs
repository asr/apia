------------------------------------------------------------------------------
-- |
-- Module      : Utils.Monad
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2014
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <asr@eafit.edu.co>
-- Stability   : experimental
--
-- Monads utilities
------------------------------------------------------------------------------

{-# LANGUAGE UnicodeSyntax #-}

module Utils.Monad
  ( die
  , failureMsg
  , pair
  ) where

------------------------------------------------------------------------------
-- Haskell imports

import System.Environment ( getProgName )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )

------------------------------------------------------------------------------
-- | Sequences a pair of monadic computations.
pair ∷ Monad m ⇒ m a → m b → m (a, b)
pair mx my = mx >>= \x → my >>= \y → return (x, y)

-- | Failure message.
failureMsg ∷ String → IO ()
failureMsg err = getProgName >>= \prg → hPutStrLn stderr $ prg ++ ": " ++ err

-- | Exit with an error message.
die ∷ String → IO a
die err = failureMsg err >> exitFailure
