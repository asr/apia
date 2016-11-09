Apia [![Build Status](https://travis-ci.org/asr/apia.svg?branch=master)](https://travis-ci.org/asr/apia)
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
  [E](http://wwwlehre.dhbw-stuttgart.de/%7Esschulz/E/E.html) | E 1.9.1-001 Sungma
  [Equinox](http://www.cse.chalmers.se/~koen/code/) | Equinox 5.0alpha (2010-06-29)
  [ileanCoP](http://www.leancop.de/ileancop/index.html) | ileanCoP 1.3beta1
  [Metis](http://www.gilith.com/software/metis/) | Metis 2.3 (release 20161108)
  [SPASS](http://www.spass-prover.org/) | SPASS 3.7
  [Vampire](http://www.vprover.org/) | Vampire 0.6 (revision 903)
  [Z3](https://github.com/Z3Prover/z3/wiki) | Z3 version 4.5.0 - 64 bit

  Moreover, Apia has support for
  [OnlineATPs](http://github.com/jonaprieto/onlineatps). With this
  feature we have freedom to use any online ATP available in the
  [TPTP World](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP).

* The tptp4X program

  The `--check` command-line option or using
  [Z3](https://github.com/Z3Prover/z3/wiki) as a first-order ATP
  require the tptp4X program from the
  [TPTP library](http://www.tptp.org). This program is included in
  this repository. The tested version of tptp4X is 6.4.0.2 from
  [Geoff's Service Tools](http://www.tptp.org/ServiceTools.tgz).

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

   In order to test the installation of Apia, you can try to
   automatically prove the conjecture in

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

   using the E ATP by running the following commands:

   ````bash
   $ agda Test.agda
   $ apia --atp=e Test.agda
   Proving the conjecture in /tmp/Test/9-8744-comm.tptp ...
   E 1.9.1-001  Sungma proved the conjecture
   ````

   Apia will call the E ATP and tell if this ATP was able to prove the
   conjecture. If the ATP could not prove the conjecture after the
   default timeout, the process of proving that particular conjecture
   is aborted.

Using online ATPs
------------------

After installing the `onlineatps` tool from
[this](http://github.com/jonaprieto/onlineatps) repository, it is
possible to use any ATP available on
[SystemOnTPTP](http://www.cs.miami.edu/~tptp/cgi-bin/SystemOnTPTP).

For example, we could use an online version of the E ATP using the
following commands:

````bash
$ apia --atp=online-e Test.agda
Proving the conjecture in /tmp/Test/9-8744-comm.tptp ...
E-2.0 proved the conjecture
````

To see a list of all online ATPs available, run the following command:

```bash
$ onlineatps --list-atps
```

YAML Configuration
------------------
<!-- Following the style to present the stack.yaml configuration files-->
We can run Apia using options set in YAML files with name `.apia`.
The options break down into project-specific options in:

  - `<project dir>/.apia`

and non-project-specific options in:

  - `~/.apia` -- for user non-project default options

*Note:* When Apia is invoked outside a project it will source project specific
options from `~/.apia`. Options in this file will be ignored for a project with
its own `<project dir>/.apia`.

### Project-specific config

Project-specific options are only valid in the `.apia` file local to a
project, not in the user config files.

> Note: We define *project* to mean a directory that contains an `.apia`
> file, which specifies valid options. The options are specified in the `help`
> command. Check `apia --help` to see all options available.

In your project-specific options, you specify  *which options to use*
when running Apia.

### Examples

We want to use the ATPs E, Metis and the online version of Vampire with Apia.
Also, we want to save the results in the directory `/myoutputdir` and use
the option `check`. Then, our Apia file should be similar to this one.

```yaml
# cat ~/.apia
atp: ["e", "metis", "online-vampire"]
output-dir: "/myoutputdir"
check: true
```

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
