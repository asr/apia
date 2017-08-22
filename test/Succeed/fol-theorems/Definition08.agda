------------------------------------------------------------------------------
-- Testing the translation of definitions
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module Definition08 where

open import Common.FOL

postulate
  P  : D → Set
  op : D → D

-- In this case the proof term `Pb` is referenced in the types of the
-- definitions of `c` and `d` via the `where` clause. Therefore in the
-- translation of `c` and `d`, we need to erase this proof term.

-- TODO (2016-04-02): This test case is invalid after fixing #22.

-- foo : D → ∀ {b} → P b → D
-- foo a Pb = a
--   where
--   c : D
--   c = a
--   {-# ATP definition c #-}

--   d : D
--   d = op c
--   {-# ATP definition d #-}

--   postulate bar : d ≡ op a
--   {-# ATP prove bar #-}

-- We need to have at least one conjecture to generate a TPTP file.
postulate bar : ∀ d → d ≡ d
{-# ATP prove bar #-}
