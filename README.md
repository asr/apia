Apia
====

Description
-----------

Apia is a Haskell program for proving first-order theorems written in
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) using automatic
theorem provers for first-order logic (ATPs). Before calling the ATPs,
the Agda formulae are translated into
[TPTP](http://www.cs.miami.edu/~tptp/) language.

Apia is used for reasoning about functional programs by combining
interactive and automatic proofs (see
[README.md](https://github.com/asr/fotc/blob/master/README.md)).

Prerequisites
--------------

* Extended version of Agda

  We have extended the development version of Agda in order to handle
  the new built-in ATP-pragma. This extended version of Agda is
  required by the Apia program.

* ATPs

  Apia requires at least one ATP. The current supported ATPs are:

  ATP | Tested version
  --- | --------------------
  [CVC4](http://cvc4.cs.nyu.edu/web/) | CVC 1.4
  [E](http://www4.informatik.tu-muenchen.de/~schulz/WORK/eprover.html) | E 1.9 Sourenee
  [Equinox](http://www.cse.chalmers.se/~koen/code/) | Equinox 5.0alpha (2010-06-29)
  [ileanCoP](http://www.leancop.de/ileancop/index.html) | ileanCoP 1.3beta1
  [Metis](http://www.gilith.com/software/metis/) | Metis 2.3 (release 20160714)
  [SPASS](http://www.spass-prover.org/) | SPASS 3.7
  [Vampire](http://www.vprover.org/) | Vampire 0.6 (revision 903)
  [Z3](https://github.com/Z3Prover/z3/wiki) | Z3 4.4.1

* The tptp4X program

  The `--check` command-line option or using Z3 as a first-order ATP
  require the tptp4X program from the
  [TPTP library](http://www.cs.miami.edu/~tptp/). The tested version
  of tptp4X is from TPTP 6.4.0.

Installation
------------

1. Extended version of Agda (see
   [README.md](https://github.com/asr/eagda/blob/master/README.md))

2. The Apia program

   You can download the Apia program using
   [Git](http://git-scm.com/). The program can be downloaded and
   installed with the following commands:

   ````bash
   $ git clone https://github.com/asr/apia.git
   $ cd apia
   $ cabal install
   ````

   In order to test the installation of the program, once the extended
   version of Agda and (some of) the ATPs have been installed, we can
   try to automatically prove the conjecture in

    ````agda
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
   E 1.9  Sourenee proved the conjecture
   ````

   The program will call the default ATPs and tell which of the ATPs
   was able to first prove a certain conjecture. If none ATP could
   prove a conjecture after four minutes, the process of proving that
   particular conjecture is aborted and the ATPs try to prove the next
   conjecture.

Command-line options
--------------------

* Choosing the ATPs

  If we want to just use a certain ATP, say Equinox, we can run the
  following command:

   ````bash
   $ apia --atp=equinox Test.agda
   Proving the conjecture in /tmp/Test/9-8744-comm.tptp ...
   Equinox, version 5.0alpha, 2010-06-29 proved the conjecture
   ````

* No using the ATPs equality

  The `_≡_` symbol is translated to the ATPs equality by default. For
  reverting this behaviour use the `--no-internal-equality` option.

Known limitations
-----------------

* Logical symbols

  The following symbols are hard-coded, i.e. they should be used: `⊥`
  (falsehood), `⊤` (truth), `¬_` (negation), `_∧_` (conjunction),
  `_∨_` (disjunction), the Agda non-dependent function type `→`
  (implication), `_↔_` (equivalence), the Agda dependent function type
  `(x : A) → B` (universal quantifier) and `∃` (existential
  quantifier).

* Agda version

  The Apia program must be compiled using the same version of Agda
  that was used to generate the Agda interface files.
