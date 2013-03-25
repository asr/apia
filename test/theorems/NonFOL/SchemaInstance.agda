------------------------------------------------------------------------------
-- Testing the translation of scheme's instances
------------------------------------------------------------------------------

{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --universal-quantified-propositional-functions #-}
{-# OPTIONS --without-K #-}

module NonFOL.SchemaInstance where

-- A schema
-- Current translation: ∀ p q x. app(p,x) → app(q,x).
postulate
  D      : Set
  schema : (A B : D → Set) → ∀ {x} → A x → B x

-- Using the current translation, the ATPs can prove an instance of
-- the schema.
postulate
  d         : D
  A B       : D → Set
  instanceC : A d → B d
{-# ATP prove instanceC schema #-}
