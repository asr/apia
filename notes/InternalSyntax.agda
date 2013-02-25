------------------------------------------------------------------------------
-- Agda internal syntax
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K #-}

-- Tested with Agda development version on 25 February 2013.

module InternalSyntax where

------------------------------------------------------------------------------
-- Sets

postulate A B C : Set

-- El {getSort = Type (Max [ClosedLevel 1]), unEl = Sort (Type (Max []))}

postulate A₁ : Set₁

-- El {getSort = Type (Max [ClosedLevel 2]), unEl = Sort (Type (Max [ClosedLevel 1]))}

------------------------------------------------------------------------------
-- The term is Def

postulate def : A

-- El {getSort = Type (Max []), unEl = Def InternalSyntax.A []}

------------------------------------------------------------------------------
-- The term is Pi

postulate fun₁ : A → B

-- El {getSort = Type (Max []),
--    unEl = Pi []r(El {getSort = Type (Max []), unEl = Def InternalSyntax.A []})
--              (NoAbs "_" El {getSort = Type (Max []), unEl = Def InternalSyntax.B []})}

postulate fun₂ : A → B → C

-- El {getSort = Type (Max []),
--    unEl = Pi []r(El {getSort = Type (Max []), unEl = Def InternalSyntax.A []})
--           (NoAbs "_" El {getSort = Type (Max []),
--                      unEl = Pi []r(El {getSort = Type (Max []), unEl = Def InternalSyntax.B []})
--                            (NoAbs "_" El {getSort = Type (Max []), unEl = Def InternalSyntax.C []})})}

postulate fun₃ : (a : A) → B

-- El (Type (Max []))
--    (Pi r(El (Type (Max []))
--             (Def InternalSyntax.A []))
--        (Abs "a" El (Type (Max []))
--                 (Def InternalSyntax.B [])))

-- El {getSort = Type (Max []),
--    unEl = Pi []r(El {getSort = Type (Max []), unEl = Def InternalSyntax.A []})
--              (NoAbs "a" El {getSort = Type (Max []), unEl = Def InternalSyntax.B []})}

postulate P : A → Set

-- El {getSort = Type (Max [ClosedLevel 1]),
--    unEl = Pi []r(El {getSort = Type (Max []), unEl = Def InternalSyntax.A []})
--              (NoAbs "_" El {getSort = Type (Max [ClosedLevel 1]), unEl = Sort (Type (Max []))})}

postulate fun₅ : (a : A) → P a

-- El {getSort = Type (Max []),
--    unEl = Pi []r(El {getSort = Type (Max []), unEl = Def InternalSyntax.A []})
--             (Abs "a" El {getSort = Type (Max []), unEl = Def InternalSyntax.P [[]r(Var 0 [])]})}
