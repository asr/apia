------------------------------------------------------------------------------
-- Testing the translation of logical schemata with 11-ary predicates symbols
------------------------------------------------------------------------------

{-# OPTIONS --schematic-propositional-functions #-}

module NonFOL.LogicalSchemata.P11 where

postulate
  D      : Set
  P-refl : {P : D → D → D → D → D → D → D → D → D → D → D → Set} →
           ∀ {x₁ x₂ x₃ x₄ x₅ x₆ x₇ x₈ x₉ x₁₀ x₁₁} →
           P x₁ x₂ x₃ x₄ x₅ x₆ x₇ x₈ x₉ x₁₀ x₁₁ →
           P x₁ x₂ x₃ x₄ x₅ x₆ x₇ x₈ x₉ x₁₀ x₁₁
{-# ATP prove P-refl #-}
