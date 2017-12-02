
-- | Apia prelude.

{-# LANGUAGE CPP           #-}
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
import Data.Functor   as YAP
import Data.Int       as YAP
import Data.List      as YAP
import Data.Maybe     as YAP
import Data.Ord       as YAP
import Data.String    as YAP
import Data.Tuple     as YAP

import Control.Applicative as YAP
import Control.Monad       as YAP

#if __GLASGOW_HASKELL__ <= 710
import Control.Monad.Reader as YAP ( MonadIO(liftIO) )
#else
import Control.Monad.IO.Class as YAP ( MonadIO(liftIO) )
#endif

import GHC.Num   as YAP ( (+), (*), (-) )
import GHC.Prim  as YAP ( seq )
import GHC.Real  as YAP ( fromIntegral, round )
import GHC.Types as YAP ( Float )

import System.IO as YAP hiding ( putStr, putStrLn )

import Text.Read as YAP ( read )
import Text.Show as YAP

------------------------------------------------------------------------------
-- Non-exported modules

import Data.Text ( Text )
import qualified Data.Text.IO as T

------------------------------------------------------------------------------
-- IO utilities

putStr ∷ MonadIO m ⇒ Text → m ()
putStr = liftIO . T.putStr

putStrLn ∷ MonadIO m ⇒ Text → m ()
putStrLn = liftIO . T.putStrLn
