# Description

  Apia is a Haskell program for proving first-order theorems written
  in [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) using
  automatic theorem provers for first-order logic (ATPs). Before
  calling the ATPs, the Agda formulae are translated into
  [TPTP](http://www.cs.miami.edu/~tptp/) language.

# Prerequisites

* Modified version of Agda

  We have modified the development version of Agda in order to handle
  the new built-in ATP pragma. This modified version of Agda is
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

  Apia uses by default the tptp4X program from the [TPTP
  library](http://www.cs.miami.edu/~tptp/). This program can be
  avoided using the `--no-check` command-line option. The tested
  version of tptp4X is from TPTP 6.0.0.

# Installation

1. Modified version of Agda

   You can download our modified version of Agda using
   [darcs](http://darcs.net/) with the following command:

   ````bash
   $ darcs get http://patch-tag.com/r/asr/magda
   ````

   This will create a directory called `magda`. Installing our
   modified version is similar to the installation of Agda (see the
   [Agda wiki](http://wiki.portal.chalmers.se/agda/pmwiki.php) for
   more information). In our setup we run the first time the following
   commands:

   ````bash
   $ cd magda
   $ autoconf
   $ ./configure
   $ make install
   ````
   After pulling new patches, we run the following commands:

   ````bash
   $ cd magda
   $ make compile-emacs-mode
   ````

   To test the installation of the modified version of Agda, type-check
   a module which uses the new built-in ATP pragma, for example

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

   Observe that in order to avoid conflicts with other installed
   versions of Agda, we have added extra information to the version
   number of Agda, i.e. if the development version number is A.B.C,
   our modified version number is A.B.C.D.

2. The Apia program

   You can download the Apia program using
   [git](http://git-scm.com/). The program can be downloaded and
   installed with the following commands:

   ````bash
   $ git clone git://github.com/asr/apia.git
   $ cd apia
   $ cabal install
   ````

   In order to test the installation of the program, once (some of)
   the ATPs have been installed, we can try to automatically prove
   all the conjectures in the file above by running the following
   command:

   ````bash
   $ apia Test.agda
   Proving the conjecture in /tmp/Test/9-8744-comm.tptp ...
   E 1.8-001 Gopaldhara proved the conjecture
   ````

   The program will call the installed ATPs and tell which of the ATPs
   was able to first prove a certain conjecture. If none ATP could
   prove a conjecture after 300 seconds, the process of proving that
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
