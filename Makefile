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
APIA = dist/build/apia/apia --check --atp=e

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
  $(call my_pathsubst,generated_fol_theorems,$(fol_theorems_path))

generated_non_fol_theorems_files = \
  $(call my_pathsubst,generated_non_fol_theorems,\
         $(non_fol_theorems_path))

generated_non_theorems_files = \
  $(call my_pathsubst,generated_non_theorems,$(non_theorems_path))

non_conjectures_files = \
  $(patsubst %.agda, %.non_conjectures,\
    $(shell find $(non_conjectures_path) -name '*.agda' | sort))

only_fol_theorems_files = \
  $(call my_pathsubst,only_fol_theorems,$(fol_theorems_path))

only_non_fol_theorems_files = \
  $(call my_pathsubst,only_non_fol_theorems,\
         $(non_fol_theorems_path))

prove_fol_theorems_files = \
  $(call my_pathsubst,prove_fol_theorems,$(fol_theorems_path))

prove_non_fol_theorems_files = \
  $(call my_pathsubst,prove_non_fol_theorems,\
         $(non_fol_theorems_path))

refute_theorems_files = \
  $(call my_pathsubst,refute_theorems,$(non_theorems_path))

# Notes

type_check_notes_files = \
  $(patsubst %.agda, %.type_check_notes, \
    $(shell find $(notes_path) -name '*.agda' | sort))

prove_notes_files = $(call my_pathsubst,prove_notes,$(notes_path))

##############################################################################
# Test suite: Generated FOL theorems

GENERATED_FOL_THEOREMS_FLAGS = \
  -v 0\
  -i$(fol_theorems_path) --only-files \
  --output-dir=$(output_dir)

%.generated_fol_theorems :
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

.PHONY : generated_fol_theorems_aux
generated_fol_theorems_aux : $(generated_fol_theorems_files)

.PHONY : generated_fol_theorems
generated_fol_theorems :
	rm -r -f $(output_dir)
	make generated_fol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-FOL theorems

GENERATED_NON_FOL_THEOREMS_FLAGS = \
  -v 0 \
  -i$(non_fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.generated_non_fol_theorems :
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

.PHONY : generated_non_fol_theorems_aux
generated_non_fol_theorems_aux : \
  $(generated_non_fol_theorems_files)

.PHONY : generated_non_fol_theorems
generated_non_fol_theorems :
	rm -r -f $(output_dir)
	make generated_non_fol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-theorems

GENERATED_NON_THEOREMS_FLAGS = \
  -v 0 \
  -i$(non_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.generated_non_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@$(APIA) $(GENERATED_NON_THEOREMS_FLAGS) $*.agda
	@diff -r $* $(output_dir)/$*

.PHONY : generated_non_theorems_aux
generated_non_theorems_aux : $(generated_non_theorems_files)

.PHONY : generated_non_theorems
generated_non_theorems : $(generated_non_theorems_files)
	rm -r -f $(output_dir)
	make generated_non_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated TPTP files

.PHONY : generated_all
generated_all :
	make generated_fol_theorems
	make generated_non_fol_theorems
	make generated_non_theorems
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only FOL theorems files

ONLY_FOL_THEOREMS_FLAGS = \
  -i$(fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.only_fol_theorems :
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

.PHONY : only_fol_theorems
only_fol_theorems : $(only_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only non-FOL theorems files

ONLY_NON_FOL_THEOREMS_FLAGS = \
  -i$(non_fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.only_non_fol_theorems :
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

.PHONY : only_non_fol_theorems
only_non_fol_theorems : \
  $(only_non_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove FOL theorems

PROVE_FOL_THEOREMS_FLAGS = \
  -i$(fol_theorems_path) \
  --output-dir=$(output_dir) \
  --time=10 \

# We don't prove ${fol_theorems_path}/Definition10.agda with Z3 due to
# Issue #25.

%.prove_fol_theorems :
	$(AGDA) -i$(fol_theorems_path) $*.agda
	@for atp in ${ATPs} ; do \
	  set -e ; \
	  case $*.agda in \
            "${fol_theorems_path}/Definition10.agda") \
              if [[ $$atp != ileancop && $$atp != z3 ]]; then \
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

.PHONY : prove_fol_theorems
prove_fol_theorems : $(prove_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove non-FOL theorems

PROVE_NON_FOL_THEOREMS_FLAGS = \
  -i$(non_fol_theorems_path) \
  --output-dir=$(output_dir) \
  --time=10

%.prove_non_fol_theorems :
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

.PHONY : prove_non_fol_theorems
prove_non_fol_theorems : \
  $(prove_non_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove all theorems

.PHONY : prove_all_theorems
prove_all_theorems :
	make prove_fol_theorems
	make prove_non_fol_theorems
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Refute theorems

%.refute_theorems :
	@echo "Processing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@if ( $(APIA) -i$(non_theorems_path) \
	              --output-dir=$(output_dir) --time=5 $*.agda ); then \
	    exit 1; \
	fi

.PHONY : refute_theorems
refute_theorems : $(refute_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Non-conjectures

%.non_conjectures :
	@$(AGDA) -i$(non_conjectures_path) $*.agda

# Tested with shelltestrunner 1.3.5.
.PHONY : non_conjectures
non_conjectures : $(non_conjectures_files)
	shelltest --color \
                  --execdir \
                  --precise \
                  $(non_conjectures_path)/non-conjectures.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Errors

# Tested with shelltestrunner 1.3.5.
.PHONY : errors
errors :
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

%.type_check_notes :
	$(AGDA) $(type_check_notes_path) $*.agda

.PHONY : type_check_notes
type_check_notes : $(type_check_notes_files)
	@echo "$@ succeeded!"

##############################################################################
# Notes: Prove theorems

prove_notes_path = -i$(notes_path) \
                   -i$(notes_path)/README

%.prove_notes :
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

.PHONY : prove_notes
prove_notes : $(prove_notes_files)
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Apia

.PHONY : tests
tests :
	make generated_all
	make non_conjectures
	make errors
	make type_check_notes
	make prove_notes
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Agda

.PHONY : agda_changed
agda_changed : clean
	cabal clean
	cabal install --only-dependencies
	cabal configure
	cabal build
	make tests
	@echo "$@ succeeded!"

##############################################################################
# Hlint test

# Requires HLint >= 1.9.36 and run `cabal build` or `cabal install`
# before.
.PHONY : hlint
hlint :
	hlint --color=never \
              --cpp-file=dist/build/autogen/cabal_macros.h \
              --cpp-include=src/Apia/ \
              src/
	@echo "$@ succeeded!"

##############################################################################
# Git : pre-commit test

.PHONY : git_pre_commit
git_pre_commit :
	fix-whitespace --check
	make hlint
	@echo "$@ succeeded!"

##############################################################################
# Apia install

.PHONY : install-bin
install-bin :
	cabal install --disable-documentation

##############################################################################
# Haskell program coverage

# TODO: Fix

hpc_html_dir = $(apia_path)/hpc

.PHONY : hpc
hpc : hpc_clean
	cd $(apia_path) && cabal clean && cabal install --ghc-option=-fhpc
	make prove_all_theorem
	make refute_theorems
	make errors
	make non_conjectures
	hpc markup --exclude=Paths_apia \
	           --destdir=$(hpc_html_dir) \
	           --srcdir=$(apia_path) \
                   apia.tix
	hpc report --exclude=Paths_apia \
                   --decl-list \
	           --srcdir=$(apia_path) \
                   apia.tix
	rm -f *.tix

.PHONY : hpc_clean
hpc_clean :
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
