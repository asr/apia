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

module Utils.Directory ( diff )
where

------------------------------------------------------------------------------
-- Haskell imports

import Data.Algorithm.Diff ( Diff(First, Second, Both), getDiff )

------------------------------------------------------------------------------
-- | Return 'True' if the files are different, otherwise the function
-- returns 'False'.
diff ∷ FilePath → FilePath → IO Bool
diff f1 f2 = do
  let areEquals ∷ [Diff a] → Bool
      areEquals []              = True
      areEquals (First _  : _)  = False
      areEquals (Second _ : _)  = False
      areEquals (Both _ _ : xs) = areEquals xs

  [l1, l2] ← mapM readFile [f1, f2]
  return $ not $ areEquals $ getDiff l1 l2
