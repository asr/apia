
-- | Utilities on IO.

{-# LANGUAGE UnicodeSyntax #-}

module Apia.Utils.IO
  ( equalFiles
  , notEqualFiles
  ) where

------------------------------------------------------------------------------

import Apia.Prelude

import Agda.Utils.Monad ( ifM )

import qualified Data.ByteString.Lazy as BL ( readFile )

------------------------------------------------------------------------------

-- | Return 'True' if the files are equals, otherwise the function
-- returns 'False'.
equalFiles ∷ FilePath → FilePath → IO Bool
equalFiles f1 f2 = liftM2 (==) (BL.readFile f1) (BL.readFile f2)

-- | Return 'True' if the files are different, otherwise the function
-- returns 'False'.
notEqualFiles ∷ FilePath → FilePath → IO Bool
notEqualFiles f1 f2 = ifM (equalFiles f1 f2) (return False) (return True)
