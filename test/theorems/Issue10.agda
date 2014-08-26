------------------------------------------------------------------------------
-- Issue in the translation
------------------------------------------------------------------------------

module Issue10 where

postulate
  D  : Set
  P' : D → Set

postulate foo : ∀ d → P' d → P' d
{-# ATP prove foo #-}
