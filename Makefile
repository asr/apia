SHELL := /bin/bash

##############################################################################
# Paths

# Tests paths.
errors_path                   = test/fail/errors
fol_theorems_path             = test/succeed/fol-theorems
hol_theorems_path             = test/succeed/hol-theorems
many_sorted_fol_theorems_path = test/succeed/many-sorted-fol-theorems
non_conjectures_path          = test/succeed/non-conjectures
non_theorems_path             = test/fail/non-theorems

# Output directory for the TPTP files.
output_dir = /tmp/apia

# Notes path.
notes_path = notes

##############################################################################
# Variables

haskell_files = $(shell find src/ -name '*.hs')

AGDA = agda -v 0 --no-sharing

# The defaults ATPs are E, Equinox and Vampire.
APIA = dist/build/apia/apia --check

# Supported ATPs.
# Missing ileancop

# TODO (04 October 2015): Missing Z3 because it cannot prove some
# theorems after we are using unique names for the translation.

FOF_ATPs = cvc4 e equinox metis spass vampire

# Supported SMTs.
TFF0_ATPs = cvc4 z3

##############################################################################
# Auxiliary functions

my_pathsubst = $(patsubst %.agda, %.$(1), \
                 $(shell find $(2) -name '*.agda' \
                         | xargs grep -l 'ATP prove' \
                         | sort))

##############################################################################
# Files

# Tests

errors_files = $(call my_pathsubst,errors,$(errors_path))

generated_fol_theorems_files = \
  $(call my_pathsubst,generated_fol_theorems,$(fol_theorems_path))

generated_hol_theorems_files = \
  $(call my_pathsubst,generated_hol_theorems,\
         $(hol_theorems_path))

generated_many_sorted_fol_theorems_files = \
  $(call my_pathsubst,generated_many_sorted_fol_theorems,\
         $(many_sorted_fol_theorems_path))

generated_non_theorems_files = \
  $(call my_pathsubst,generated_non_theorems,$(non_theorems_path))

non_conjectures_files = \
  $(patsubst %.agda, %.non_conjectures,\
    $(shell find $(non_conjectures_path) -name '*.agda' | sort))

only_fol_theorems_files = \
  $(call my_pathsubst,only_fol_theorems,$(fol_theorems_path))

only_hol_theorems_files = \
  $(call my_pathsubst,only_hol_theorems,\
         $(hol_theorems_path))

prove_fol_theorems_files = \
  $(call my_pathsubst,prove_fol_theorems,$(fol_theorems_path))

prove_hol_theorems_files = \
  $(call my_pathsubst,prove_hol_theorems,\
         $(hol_theorems_path))

prove_many_sorted_fol_theorems_files = \
  $(call my_pathsubst,prove_many_sorted_fol_theorems,\
         $(many_sorted_fol_theorems_path))

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
  -v 0 \
  -i$(fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)/$(fol_theorems_path)

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

generated_fol_theorems_aux : $(generated_fol_theorems_files)

generated_fol_theorems :
	rm -r -f $(output_dir)
	make generated_fol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated HOL theorems

GENERATED_HOL_THEOREMS_FLAGS = \
  -v 0 \
  -i$(hol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)/$(hol_theorems_path)

%.generated_hol_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(hol_theorems_path) $*.agda
	@case $*.agda in \
          "${hol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
          "${hol_theorems_path}/Eta-Issue8.agda" | \
          "${hol_theorems_path}/Existential.agda" | \
          "${hol_theorems_path}/Instance.agda" | \
          "${hol_theorems_path}/Issue12.agda" | \
          "${hol_theorems_path}/OptionsLList.agda" | \
          "${hol_theorems_path}/P11.agda" | \
          "${hol_theorems_path}/PropositionalFunction.agda") \
	    $(APIA) $(GENERATED_HOL_THEOREMS_FLAGS) \
	            --schematic-propositional-functions \
	            $*.agda \
            ;; \
          "${hol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) $(GENERATED_HOL_THEOREMS_FLAGS) \
	            --schematic-propositional-symbols \
	            $*.agda \
            ;; \
          *) exit 1 \
             ;; \
        esac
	@diff -r $* $(output_dir)/$*

generated_hol_theorems_aux : \
  $(generated_hol_theorems_files)

generated_hol_theorems :
	rm -r -f $(output_dir)
	make generated_hol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated many-sorted FOL theorems

GENERATED_MANY_SORTED_FOL_THEOREMS_FLAGS = \
  -v 0 \
  -i$(many_sorted_fol_theorems_path) \
  --only-files \
  --lang=tff0 \
  --output-dir=$(output_dir)/$(many_sorted_fol_theorems_path)

%.generated_many_sorted_fol_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(many_sorted_fol_theorems_path) $*.agda
	$(APIA) $(GENERATED_MANY_SORTED_FOL_THEOREMS_FLAGS) $*.agda
	@diff -r $* $(output_dir)/$*

generated_many_sorted_fol_theorems_aux : \
  $(generated_many_sorted_fol_theorems_files)

generated_many_sorted_fol_theorems :
	rm -r -f $(output_dir)
	make generated_many_sorted_fol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-theorems

GENERATED_NON_THEOREMS_FLAGS = \
  -v 0 \
  -i$(non_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)/$(non_theorems_path)

%.generated_non_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@$(APIA) $(GENERATED_NON_THEOREMS_FLAGS) $*.agda
	@diff -r $* $(output_dir)/$*

generated_non_theorems_aux : $(generated_non_theorems_files)

generated_non_theorems : $(generated_non_theorems_files)
	rm -r -f $(output_dir)
	make generated_non_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated TPTP files

generated_all :
	make generated_fol_theorems
	make generated_hol_theorems
	make generated_many_sorted_fol_theorems
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

only_fol_theorems : $(only_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only HOL theorems files

ONLY_HOL_THEOREMS_FLAGS = \
  -i$(hol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)

%.only_hol_theorems :
	$(AGDA) -i$(hol_theorems_path) $*.agda
	@case $*.agda in \
          "${hol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
          "${hol_theorems_path}/Eta-Issue8.agda" | \
          "${hol_theorems_path}/Existential.agda" | \
          "${hol_theorems_path}/Instance.agda" | \
          "${hol_theorems_path}/Issue12.agda" | \
          "${hol_theorems_path}/OptionsLList.agda" | \
          "${hol_theorems_path}/P11.agda" | \
          "${hol_theorems_path}/PropositionalFunction.agda") \
	    $(APIA) ${ONLY_HOL_THEOREMS_FLAGS} \
	            --schematic-propositional-functions \
                    $*.agda \
            ;; \
          "${hol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) ${ONLY_HOL_THEOREMS_FLAGS} \
	            --schematic-propositional-symbols \
                    $*.agda \
            ;; \
          *) exit 1 \
             ;; \
        esac

only_hol_theorems : \
  $(only_hol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove FOL theorems

PROVE_FOL_THEOREMS_FLAGS = \
  -i$(fol_theorems_path) \
  --output-dir=$(output_dir) \
  --time=10

%.prove_fol_theorems :
	$(AGDA) -i$(fol_theorems_path) $*.agda
	@for atp in ${FOF_ATPs} ; do \
	  case $*.agda in \
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

prove_fol_theorems : $(prove_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove HOL theorems

PROVE_HOL_THEOREMS_FLAGS = \
  -i$(hol_theorems_path) \
  --output-dir=$(output_dir) \
  --time=10

%.prove_hol_theorems :
	$(AGDA) -i$(hol_theorems_path) $*.agda
	@for atp in ${FOF_ATPs} ; do \
	  case $*.agda in \
            "${hol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
            "${hol_theorems_path}/Eta-Issue8.agda" | \
            "${hol_theorems_path}/Existential.agda" | \
            "${hol_theorems_path}/Instance.agda" | \
            "${hol_theorems_path}/Issue12.agda" | \
            "${hol_theorems_path}/OptionsLList.agda" | \
            "${hol_theorems_path}/P11.agda" | \
            "${hol_theorems_path}/PropositionalFunction.agda") \
	      $(APIA) ${PROVE_HOL_THEOREMS_FLAGS} \
                      --atp=$$atp \
                      --schematic-propositional-functions \
		      $*.agda ; \
            ;; \
            "${hol_theorems_path}/PropositionalSymbol.agda") \
	      $(APIA) ${PROVE_HOL_THEOREMS_FLAGS} \
                      --atp=$$atp \
                      --schematic-propositional-symbols \
		      $*.agda ; \
            ;; \
            *) exit 1 \
               ;; \
          esac ; \
        done

prove_hol_theorems : \
  $(prove_hol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove many-sorted FOL theorems

PROVE_MANY_SORTED_FOL_THEOREMS_FLAGS = \
  -i$(many_sorted_fol_theorems_path) \
  --lang=tff0 \
  --output-dir=$(output_dir) \
  --time=10

%.prove_many_sorted_fol_theorems :
	$(AGDA) -i$(many_sorted_fol_theorems_path) $*.agda
	@for atp in ${TFF0_ATPs} ; do \
          $(APIA) ${PROVE_MANY_SORTED_FOL_THEOREMS_FLAGS} \
                  --atp=$$atp \
                  $*.agda ; \
        done

prove_many_sorted_fol_theorems : $(prove_many_sorted_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove all theorems

prove_all_theorems :
	make prove_fol_theorems
	make prove_hol_theorems
	make prove_many_sorted_fol_theorems
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

refute_theorems : $(refute_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Non-conjectures

%.non_conjectures :
	@$(AGDA) -i$(non_conjectures_path) $*.agda

# Tested with shelltestrunner 1.3.5.
non_conjectures : $(non_conjectures_files)
	shelltest --color --precise \
                  $(non_conjectures_path)/non-conjectures.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Error messages

%.errors :
	@$(AGDA) -i$(errors_path) $*.agda

# Tested with shelltestrunner 1.3.5.
errors : $(errors_files)
	shelltest --color --precise $(errors_path)/errors.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Haddock test
#
# Tested with cabal-install version 1.20.0.3 using version 1.20.0.2 of
# the Cabal library.

doc :
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

type_check_notes : $(type_check_notes_files)
	@echo "$@ succeeded!"

##############################################################################
# Notes: Prove theorems

prove_notes_path = -i$(notes_path) \
                   -i$(notes_path)/README

%.prove_notes :
	echo $(prove_notes_files)
	$(AGDA) $(prove_notes_path) $*.agda
	@for atp in ${FOF_ATPs} ; do \
          $(APIA) $(prove_notes_path) \
                  --atp=$$atp \
	          --output-dir=$(output_dir) \
	          --time=10 \
	          $*.agda ; \
        done

prove_notes : $(prove_notes_files)
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Agda

agda_changed :
	make apia_changed
	make type_check_notes
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Apia

apia_changed : clean
	cabal clean
	cabal install --only-dependencies
	cabal configure
	cabal build
	make generated_all
	make non_conjectures
	make errors
	make prove_notes
	@echo "$@ succeeded!"

##############################################################################
# Hlint test

# Due to HLint Issue 196, the `-XNoRoleAnnotations` option is required.
hlint :
	hlint --color=never \
              --cpp-file=dist/build/autogen/cabal_macros.h \
              --cpp-include=src/Apia/ \
              -XNoRoleAnnotations \
              src/
	@echo "$@ succeeded!"

##############################################################################
# Git : pre-commit test

git_pre_commit :
	fix-whitespace --check
	make doc
	make hlint
	@echo "$@ succeeded!"

##############################################################################
# Apia install

install-bin :
	cabal install --disable-documentation

##############################################################################
# Haskell program coverage

# TODO: Fix

hpc_html_dir = $(apia_path)/hpc

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

hpc_clean :
	rm -f *.tix
	rm -f -r $(hpc_html_dir)

##############################################################################
# Others

.PHONY : TAGS
TAGS :
	hTags -I src/Apia/ \
              -i dist/build/autogen/cabal_macros.h \
              -e $(haskell_files)

TODO :
	find . -type d \( -path './.git' -o -path './dist' \) -prune -o -print \
	| xargs grep -I 'TODO' \
	| sort

clean :
	find . -type f -name '*.agdai' -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.o' -delete
	find . -type f -name 'apia.tix' -delete
	find . -type f -name 'model' -delete
	rm -f -r $(output_dir)
