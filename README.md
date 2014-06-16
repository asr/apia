# Description

  Apia is a Haskell program for proving first-order theorems written
  in [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) using
  automatic theorem provers for first-order logic (ATPs). Before
  calling the ATPs, the Agda formulae are translated into
  [TPTP](http://www.cs.miami.edu/~tptp/) language.

# Use of Apia

  * Reasoning about Functional Programs by Combining Interactive and
    Automatic Proofs
    ([README.md](https://github.com/asr/fotc/blob/master/README.md)).

# Prerequisites

* Modified version of Agda

  We have modified the development version of Agda in order to handle
  the new built-in ATP-pragma. This modified version of Agda is
  required by the Apia program.

* ATPs

  Apia requires at least one of the following ATPs:
  [E](http://www4.informatik.tu-muenchen.de/~schulz/WORK/eprover.html),
  [Equinox](http://www.cse.chalmers.se/~koen/code/),
  [ileanCoP](http://www.leancop.de/ileancop/index.html),
  [Metis](http://www.gilith.com/software/metis/),
  [SPASS](http://www.spass-prover.org/) or
  [Vampire](http://www.vprover.org/). The tested versions of the ATPs
  are: E 1.8-001 Gopaldhara, Equinox 5.0alpha (2010-06-29), ileanCoP
  1.3beta1, Metis 2.3 (release 20120927), SPASS 3.7 and Vampire 0.6
  (revision 903).

* The tptp4X program

  The `--check` command-line option requires the tptp4X program from
  the [TPTP library](http://www.cs.miami.edu/~tptp/). The tested
  version of tptp4X is from TPTP 6.0.0.

# Installation

1. Modified version of Agda (see
   [README.md](https://github.com/asr/magda/blob/master/README.md))

2. The Apia program

   You can download the Apia program using
   [Git](http://git-scm.com/). The program can be downloaded and
   installed with the following commands:

   ````bash
   $ git clone https://github.com/asr/apia.git
   $ cd apia
   $ cabal install
   ````

   In order to test the installation of the program, once the modified
   version of Agda and (some of)the ATPs have been installed, we can
   try to automatically prove the conjecture in

   ````Agda
   module Test where

   data _∨_ (A B : Set) : Set where
     inj₁ : A → A ∨ B
     inj₂ : B → A ∨ B

   postulate
     A B    : Set
     ∨-comm : A ∨ B → B ∨ A
   {-# ATP prove ∨-comm #-}
   ````

   by running the following commands:

   ````bash
   $ agda Test.agda
   $ apia Test.agda
   Proving the conjecture in /tmp/Test/9-8744-comm.tptp ...
   E 1.8-001 Gopaldhara proved the conjecture
   ````

   The program will call the default ATPs and tell which of the ATPs
   was able to first prove a certain conjecture. If none ATP could
   prove a conjecture after four minutes, the process of proving that
   particular conjecture is aborted and the ATPs try to prove the next
   conjecture.

   If we want to just use a certain ATP, say Equinox, we can instead
   run the following command:

   ````bash
   $ apia --atp=equinox Test.agda
   Proving the conjecture in /tmp/Test/9-8744-comm.tptp ...
   Equinox, version 5.0alpha, 2010-06-29 proved the conjecture
   ````

# Known limitations

* Logical symbols

  The following symbols are hard-coded, i.e. they should be used: `⊥`
  (falsehood), `⊤` (truth), `¬_` (negation), `_∧_` (conjunction),
  `_∨_` (disjunction), the Agda non-dependent function type `→`
  (implication), `_↔_` (equivalence), the Agda dependent function type
  `(x : A) → B` (universal quantifier), `∃` (existential quantifier),
  and `_≡_` (propositional equality).

* Agda version

  The Apia program must be compiled using the same version of Agda
  that was used to generate the Agda interface files.
