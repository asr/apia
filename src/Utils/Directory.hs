------------------------------------------------------------------------------
-- |
-- Module      : Utils.Directory
-- Copyright   : (c) Andrés Sicard-Ramírez 2009-2013
-- License     : See the file LICENSE.
--
-- Maintainer  : Andrés Sicard-Ramírez <andres.sicard.ramirez@gmail.com>
-- Stability   : experimental
--
-- Utilities on directories.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module Utils.Directory ( equalFiles, notEqualFiles )
where

------------------------------------------------------------------------------
-- Haskell imports

import Control.Monad ( liftM2 )

------------------------------------------------------------------------------

-- | Return 'True' if the files are equals, otherwise the function
-- returns 'False'.
equalFiles ∷ FilePath → FilePath → IO Bool
equalFiles f1 f2 = liftM2 (==) (readFile f1) (readFile f2)

-- | Return 'True' if the files are different, otherwise the function
-- returns 'False'.
notEqualFiles ∷ FilePath → FilePath → IO Bool
notEqualFiles f1 f2 = liftM2 (/=) (readFile f1) (readFile f2)
