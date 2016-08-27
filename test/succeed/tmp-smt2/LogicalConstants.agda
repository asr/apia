------------------------------------------------------------------------------
-- Testing the translation of the logical constants
------------------------------------------------------------------------------

{-# OPTIONS --exact-split              #-}
{-# OPTIONS --no-sized-types           #-}
{-# OPTIONS --no-universe-polymorphism #-}
{-# OPTIONS --without-K                #-}

module LogicalConstants where

infix  5 ¬_
infixr 4 _∧_
infixr 3 _∨_
infixr 2 _⇒_
infixr 1 _↔_ _⇔_

------------------------------------------------------------------------------
-- Propositional logic

-- The logical constants

-- The logical constants are hard-coded in our implementation,
-- i.e. the following symbols must be used (it is possible to use Agda
-- non-dependent function space → instead of ⇒).
postulate
  ⊥ ⊤         : Set -- N.B. the name of the tautology symbol is "\top" not T.
  ¬_          : Set → Set -- N.B. the right hole.
  _∧_ _∨_     : Set → Set → Set
  _⇒_ _↔_ _⇔_ : Set → Set → Set

-- We postulate some formulae (which are translated as 0-ary
-- predicates).
postulate A B C : Set

-- Testing the conditional using the non-dependent function type.
postulate A→A : A → A
{-# ATP prove A→A #-}

-- The introduction and elimination rules for the propositional
-- connectives are theorems.
postulate
  →I  : (A → B) → A ⇒ B
  →E  : (A ⇒ B) → A → B
  ∧I  : A → B → A ∧ B
  ∧E₁ : A ∧ B → A
  ∧E₂ : A ∧ B → B
  ∨I₁ : A → A ∨ B
  ∨I₂ : B → A ∨ B
  ∨E  : (A ⇒ C) → (B ⇒ C) → A ∨ B → C
  ⊥E  : ⊥ → A
  ¬E : (¬ A → ⊥) → A
-- {-# ATP prove →I #-}
-- {-# ATP prove →E #-}
-- {-# ATP prove ∧I #-}
-- {-# ATP prove ∧E₁ #-}
-- {-# ATP prove ∧E₂ #-}
-- {-# ATP prove ∨I₁ #-}
-- {-# ATP prove ∨I₂ #-}
-- {-# ATP prove ∨E #-}
-- {-# ATP prove ⊥E #-}
-- {-# ATP prove ¬E #-}
