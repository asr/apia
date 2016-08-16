
-- | Apia prelude.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Prelude
  ( module YAP
  , putStr
  , putStrLn
  ) where

------------------------------------------------------------------------------
-- Exported modules

import Data.Bool as YAP

import Data.Char as YAP
  ( Char
  , chr
  , isAsciiLower
  , isAsciiUpper
  , isDigit
  , isUpper
  , ord
  , toUpper
  )

import Data.Either    as YAP
import Data.Eq        as YAP
import Data.Function  as YAP
import Data.Int       as YAP
import Data.List      as YAP
import Data.Maybe     as YAP
import Data.Ord       as YAP
import Data.String    as YAP
import Data.Tuple     as YAP

import Control.Applicative as YAP
import Control.Monad       as YAP

import GHC.Num   as YAP ( (+), (*), (-) )
import GHC.Real  as YAP ( fromIntegral, round )
import GHC.Types as YAP ( Float )

import System.IO as YAP hiding ( putStr, putStrLn )

import Text.Read as YAP ( read )
import Text.Show as YAP

------------------------------------------------------------------------------
-- Internal use

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.Text ( Text )
import qualified Data.Text.IO as T

------------------------------------------------------------------------------
-- IO utilities

putStr ∷ MonadIO m ⇒ Text → m ()
putStr = liftIO . T.putStr

putStrLn ∷ MonadIO m ⇒ Text → m ()
putStrLn = liftIO . T.putStrLn
