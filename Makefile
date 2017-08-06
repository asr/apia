SHELL := /bin/bash

##############################################################################
# Paths

# Tests paths.
errors_path           = test/fail/errors
fol_theorems_path     = test/succeed/fol-theorems
non_conjectures_path  = test/succeed/non-conjectures
non_fol_theorems_path = test/succeed/non-fol-theorems
non_theorems_path     = test/fail/non-theorems

# Output directory for the TPTP files.
output_dir = /tmp/apia

# Notes path.
notes_path = notes

##############################################################################
# Variables

haskell_files = $(shell find src/ -name '*.hs')

AGDA = agda -v 0
APIA = dist/build/apia/apia --check

# Supported ATPs.
ATPs = cvc4 e equinox ileancop metis spass vampire z3

##############################################################################
# Auxiliary functions

my_pathsubst = $(patsubst %.agda, %.$(1), \
                 $(shell find $(2) -name '*.agda' \
                         | xargs grep -l 'ATP prove' \
                         | sort))

##############################################################################
# Files

# Tests

generated_fol_theorems_files = \
  $(call my_pathsubst,generated-fol-theorems,$(fol_theorems_path))

generated_non_fol_theorems_files = \
  $(call my_pathsubst,generated-non-fol-theorems,\
         $(non_fol_theorems_path))

generated_non_theorems_files = \
  $(call my_pathsubst,generated-non-theorems,$(non_theorems_path))

non_conjectures_files = \
  $(patsubst %.agda, %.non-conjectures,\
    $(shell find $(non_conjectures_path) -name '*.agda' | sort))

only_fol_theorems_files = \
  $(call my_pathsubst,only-fol-theorems,$(fol_theorems_path))

only_non_fol_theorems_files = \
  $(call my_pathsubst,only-non-fol-theorems,\
         $(non_fol_theorems_path))

prove_fol_theorems_files = \
  $(call my_pathsubst,prove-fol-theorems,$(fol_theorems_path))

prove_non_fol_theorems_files = \
  $(call my_pathsubst,prove-non-fol-theorems,\
         $(non_fol_theorems_path))

refute_theorems_files = \
  $(call my_pathsubst,refute-theorems,$(non_theorems_path))

errors_files = \
  $(patsubst %.agda, %.errors,\
    $(shell find $(errors_path) -name '*.agda' | sort))

# Notes

type_check_notes_files = \
  $(patsubst %.agda, %.type-check-notes, \
    $(shell find $(notes_path) -name '*.agda' | sort))

prove_notes_files = $(call my_pathsubst,prove-notes,$(notes_path))

##############################################################################
# Test suite variables

APIA_TEST_BIN = ../dist/build/apia-tests/apia-tests
TESTS_OPTIONS =-i

##############################################################################
# Test suite: Generated FOL theorems

GENERATED_FOL_THEOREMS_FLAGS = \
  -v 0\
  -i$(fol_theorems_path) --only-files \
  --output-dir=$(output_dir)

%.generated-fol-theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(fol_theorems_path) $*.agda
	@case $*.agda in \
          "${fol_theorems_path}/NonInternalEquality.agda") \
	    $(APIA) $(GENERATED_FOL_THEOREMS_FLAGS) \
                     --no-internal-equality \
                     $*.agda \
            ;; \
          *) $(APIA) $(GENERATED_FOL_THEOREMS_FLAGS) \
                     $*.agda \
             ;; \
        esac
	@diff -r $* $(output_dir)/$*

.PHONY : generated-fol-theorems-aux
generated-fol-theorems-aux : $(generated_fol_theorems_files)

.PHONY : generated-fol-theorems
generated-fol-theorems :
	rm -r -f $(output_dir)
	make generated-fol-theorems-aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-FOL theorems

GENERATED_NON_FOL_THEOREMS_FLAGS = \
  -v 0 \
  -i$(non_fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.generated-non-fol-theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(non_fol_theorems_path) $*.agda
	@case $*.agda in \
          "${non_fol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
          "${non_fol_theorems_path}/Eta-Issue8.agda" | \
          "${non_fol_theorems_path}/Existential.agda" | \
          "${non_fol_theorems_path}/Instance.agda" | \
          "${non_fol_theorems_path}/Issue12.agda" | \
          "${non_fol_theorems_path}/OptionsLList.agda" | \
          "${non_fol_theorems_path}/P11.agda" | \
          "${non_fol_theorems_path}/PropositionalFunction.agda") \
	    $(APIA) $(GENERATED_NON_FOL_THEOREMS_FLAGS) \
	            --schematic-propositional-functions \
	            $*.agda \
            ;; \
          "${non_fol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) $(GENERATED_NON_FOL_THEOREMS_FLAGS) \
	            --schematic-propositional-symbols \
	            $*.agda \
            ;; \
          *) exit 1 \
             ;; \
        esac
	@diff -r $* $(output_dir)/$*

.PHONY : generated-non-fol-theorems-aux
generated-non-fol-theorems-aux : \
  $(generated_non_fol_theorems_files)

.PHONY : generated-non-fol-theorems
generated-non-fol-theorems :
	rm -r -f $(output_dir)
	make generated-non-fol-theorems-aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-theorems

GENERATED_NON_THEOREMS_FLAGS = \
  -v 0 \
  -i$(non_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.generated-non-theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@$(APIA) $(GENERATED_NON_THEOREMS_FLAGS) $*.agda
	@diff -r $* $(output_dir)/$*

.PHONY : generated-non-theorems-aux
generated-non-theorems-aux : $(generated_non_theorems_files)

.PHONY : generated-non-theorems
generated-non-theorems : $(generated_non_theorems_files)
	rm -r -f $(output_dir)
	make generated-non-theorems-aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated TPTP files

.PHONY : generated-all
generated-all :
	make generated-fol-theorems
	make generated-non-fol-theorems
	make generated-non-theorems
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only FOL theorems files

ONLY_FOL_THEOREMS_FLAGS = \
  -i$(fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.only-fol-theorems :
	$(AGDA) -i$(fol_theorems_path) $*.agda
	@case $*.agda in \
          "${fol_theorems_path}/NonInternalEquality.agda") \
	    $(APIA) ${ONLY_FOL_THEOREMS_FLAGS} \
                    --no-internal-equality \
                    $*.agda \
            ;; \
          *) $(APIA) ${ONLY_FOL_THEOREMS_FLAGS} \
                     $*.agda \
             ;; \
         esac

.PHONY : only-fol-theorems
only-fol-theorems : $(only_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only non-FOL theorems files

ONLY_NON_FOL_THEOREMS_FLAGS = \
  -i$(non_fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.only-non-fol-theorems :
	$(AGDA) -i$(non_fol_theorems_path) $*.agda
	@case $*.agda in \
          "${non_fol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
          "${non_fol_theorems_path}/Eta-Issue8.agda" | \
          "${non_fol_theorems_path}/Existential.agda" | \
          "${non_fol_theorems_path}/Instance.agda" | \
          "${non_fol_theorems_path}/Issue12.agda" | \
          "${non_fol_theorems_path}/OptionsLList.agda" | \
          "${non_fol_theorems_path}/P11.agda" | \
          "${non_fol_theorems_path}/PropositionalFunction.agda") \
	    $(APIA) ${ONLY_NON_FOL_THEOREMS_FLAGS} \
	            --schematic-propositional-functions \
                    $*.agda \
            ;; \
          "${non_fol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) ${ONLY_NON_FOL_THEOREMS_FLAGS} \
	            --schematic-propositional-symbols \
                    $*.agda \
            ;; \
          *) exit 1 \
             ;; \
        esac

.PHONY : only-non-fol-theorems
only-non-fol-theorems : \
  $(only_non_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove FOL theorems

PROVE_FOL_THEOREMS_FLAGS = \
  -i$(fol_theorems_path) \
  --output-dir=$(output_dir) \
  --time=10 \

%.prove-fol-theorems :
	$(AGDA) -i$(fol_theorems_path) $*.agda
	@for atp in ${ATPs} ; do \
	  set -e ; \
	  case $*.agda in \
            "${fol_theorems_path}/Definition10.agda") \
              if [[ $$atp != ileancop ]]; then \
                 $(APIA) ${PROVE_FOL_THEOREMS_FLAGS} \
                         --atp=$$atp \
                         $*.agda ; \
              fi \
            ;; \
            "${fol_theorems_path}/LogicalConstants.agda") \
              if [[ $$atp != ileancop ]]; then \
                 $(APIA) ${PROVE_FOL_THEOREMS_FLAGS} \
                         --atp=$$atp \
                         $*.agda ; \
              fi \
            ;; \
            "${fol_theorems_path}/NonInternalEquality.agda") \
              $(APIA) ${PROVE_FOL_THEOREMS_FLAGS} \
                      --atp=$$atp \
                      --no-internal-equality \
                      $*.agda ; \
            ;; \
            *) $(APIA) ${PROVE_FOL_THEOREMS_FLAGS} \
                       --atp=$$atp \
                       $*.agda ; \
               ;; \
          esac ; \
        done

.PHONY : prove-fol-theorems
prove-fol-theorems : $(prove_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove non-FOL theorems

PROVE_NON_FOL_THEOREMS_FLAGS = \
  -i$(non_fol_theorems_path) \
  --output-dir=$(output_dir) \
  --time=10

%.prove-non-fol-theorems :
	$(AGDA) -i$(non_fol_theorems_path) $*.agda
	@for atp in ${ATPs} ; do \
	  set -e ; \
	  case $*.agda in \
            "${non_fol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
            "${non_fol_theorems_path}/Eta-Issue8.agda" | \
            "${non_fol_theorems_path}/Existential.agda" | \
            "${non_fol_theorems_path}/Instance.agda" | \
            "${non_fol_theorems_path}/Issue12.agda" | \
            "${non_fol_theorems_path}/OptionsLList.agda" | \
            "${non_fol_theorems_path}/P11.agda" | \
            "${non_fol_theorems_path}/PropositionalFunction.agda") \
	      $(APIA) ${PROVE_NON_FOL_THEOREMS_FLAGS} \
                      --atp=$$atp \
                      --schematic-propositional-functions \
		      $*.agda ; \
            ;; \
            "${non_fol_theorems_path}/PropositionalSymbol.agda") \
	      $(APIA) ${PROVE_NON_FOL_THEOREMS_FLAGS} \
                      --atp=$$atp \
                      --schematic-propositional-symbols \
		      $*.agda ; \
            ;; \
            *) exit 1 \
               ;; \
          esac ; \
        done

.PHONY : prove-non-fol-theorems
prove-non-fol-theorems : \
  $(prove_non_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove all theorems

.PHONY : prove-all-theorems
prove-all-theorems :
	make prove-fol-theorems
	make prove-non-fol-theorems
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Refute theorems

%.refute-theorems :
	@echo "Processing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@if ( $(APIA) -i$(non_theorems_path) \
	              --output-dir=$(output_dir) --time=5 $*.agda ); then \
	    exit 1; \
	fi

.PHONY : refute-theorems
refute-theorems : $(refute_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Non-conjectures

%.non-conjectures :
	@$(AGDA) -i$(non_conjectures_path) $*.agda

.PHONY : non-conjectures
non-conjectures : $(non_conjectures_files)
	cd test && \
	$(APIA_TEST_BIN) $(TESTS_OPTIONS) --regex-include succeed
	@echo "$@ succeeded"

# Tested with shelltestrunner 1.3.5.
.PHONY : non-conjectures-shelltestrunner
non-conjectures-shelltestrunner : $(non_conjectures_files)
	shelltest --color \
                  --execdir \
                  --precise \
                  $(non_conjectures_path)/non-conjectures.test
	@echo "$@ succeeded!"


##############################################################################
# Test suite: Errors

%.errors :
	$(AGDA) -v 0 -i$(errors_path) $*.agda

# Tested with shelltestrunner 1.3.5.
.PHONY : errors
errors : $(errors_files)
	shelltest --color --execdir --precise  $(errors_path)/errors.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Haddock test
#
# Tested with cabal-install version 1.20.0.3 using version 1.20.0.2 of
# the Cabal library.

.PHONY : haddock
haddock :
	cabal configure
	cabal haddock --executables \
	              --haddock-option=--use-unicode \
	              --hyperlink-source
	@echo "$@ succeeded!"

##############################################################################
# Notes: Type-checking

type_check_notes_path = -i$(notes_path) \
                        -i$(notes_path)/agda-interface \
                        -i$(notes_path)/README

%.type-check-notes :
	$(AGDA) $(type_check_notes_path) $*.agda

.PHONY : type-check-notes
type-check-notes : $(type_check_notes_files)
	@echo "$@ succeeded!"

##############################################################################
# Notes: Prove theorems

prove_notes_path = -i$(notes_path) \
                   -i$(notes_path)/README

%.prove-notes :
	echo $(prove_notes_files)
	$(AGDA) $(prove_notes_path) $*.agda
	@for atp in ${ATPs} ; do \
	  set -e ; \
          $(APIA) $(prove_notes_path) \
                  --atp=$$atp \
	          --output-dir=$(output_dir) \
	          --time=10 \
	          $*.agda ; \
        done

.PHONY : prove-notes
prove-notes : $(prove_notes_files)
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Apia

.PHONY : tests
tests :
	make generated-all
	make non-conjectures
	make non-conjectures-shelltestrunner
	make errors
	make type-check-notes
	make prove-notes
	make haddock
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Agda

.PHONY : agda-changed
agda-changed : clean
	cabal clean
	make install-bin
	make tests
	@echo "$@ succeeded!"

##############################################################################
# Hlint test

# Requires HLint >= 1.9.36 and run `cabal build` or `cabal install`
# before.
.PHONY : hlint
hlint :
	hlint --color=never Setup.hs
	hlint --color=never \
              --cpp-file=dist/build/autogen/cabal_macros.h \
              --cpp-include=src/Apia/ \
              src/ test/
	@echo "$@ succeeded!"

##############################################################################
# Git : pre-commit test

.PHONY : git-pre-commit
git-pre-commit :
	fix-whitespace --check
	make hlint
	@echo "$@ succeeded!"

##############################################################################
# Apia install

.PHONY : install-bin
install-bin :
	cabal install --enable-tests --disable-documentation

##############################################################################
# Haskell program coverage

# TODO: Fix

hpc_html_dir = $(apia_path)/hpc

.PHONY : hpc
hpc : hpc-clean
	cd $(apia_path) && cabal clean && cabal install --ghc-option=-fhpc
	make prove_all_theorem
	make refute-theorems
	make errors
	make non-conjectures
	make non-conjectures-shelltestrunner
	hpc markup --exclude=Paths_apia \
	           --destdir=$(hpc_html_dir) \
	           --srcdir=$(apia_path) \
                   apia.tix
	hpc report --exclude=Paths_apia \
                   --decl-list \
	           --srcdir=$(apia_path) \
                   apia.tix
	rm -f *.tix

.PHONY : hpc-clean
hpc-clean :
	rm -f *.tix
	rm -f -r $(hpc_html_dir)

##############################################################################
# White-spaces stuff

.PHONY : install-fix-whitespace
install-fix-whitespace :
	cd src/fix-whitespace && cabal install

.PHONY : check-whitespace
check-whitespace :
	fix-whitespace --check

##############################################################################
# Others

.PHONY : TAGS
TAGS :
	hTags -I src/Apia/ \
              -i dist/build/autogen/cabal_macros.h \
              -e $(haskell_files)

.PHONY : TODO
TODO :
	find . -type d \( -path './.git' -o -path './dist' \) -prune -o -print \
	| xargs grep -I 'TODO' \
	| sort

.PHONY : clean
clean :
	find . -type f -name '*.agdai' -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.o' -delete
	find . -type f -name 'apia.tix' -delete
	find . -type f -name 'model' -delete
	rm -f -r $(output_dir)
