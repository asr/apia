SHELL := /bin/bash

##############################################################################
# Paths

# Tests paths.
fol_theorems_path         =  test/succeed/fol-theorems
non_fol_theorems_path     = test/succeed/non-fol-theorems
non_theorems_path         = test/fail/non-theorems
errors_path               = test/fail/errors
command_line_options_path = test/command-line-options

# Output directory for the TPTP files.
output_dir = /tmp/apia

# Notes path.
notes_path = notes

##############################################################################
# Variables

haskell_files = $(shell find src/ -name '*.hs')

AGDA = agda -v 0

# The defaults ATPs are E, Equinox and Vampire.
APIA = dist/build/apia/apia --check
# APIA = dist/build/apia/apia --check --atp=e
# APIA = dist/build/apia/apia --check --atp=equinox
# APIA = dist/build/apia/apia --check --atp=ileancop
# APIA = dist/build/apia/apia --check --atp=metis
# APIA = dist/build/apia/apia --check --atp=spass
# APIA = dist/build/apia/apia --check --atp=vampire

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

errors_files = $(call my_pathsubst,errors,$(errors_path))

command_line_options_files = \
  $(patsubst %.agda, %.command_line_options,\
    $(shell find $(command_line_options_path) -name '*.agda' | sort))

# Notes

type_check_notes_files = \
  $(patsubst %.agda, %.type_check_notes, \
    $(shell find $(notes_path) -name '*.agda' | sort))

prove_notes_files = $(call my_pathsubst,prove_notes,$(notes_path))

##############################################################################
# Test suite: Generated FOL theorems

flags_generated_fol_theorems = \
  -i$(fol_theorems_path) --only-files \
  --output-dir=$(output_dir)/$(fol_theorems_path)

%.generated_fol_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(fol_theorems_path) $*.agda
	@$(APIA) -v 0 $(flags_generated_fol_theorems) $*.agda
	@diff -r $* $(output_dir)/$*

generated_fol_theorems_aux : $(generated_fol_theorems_files)

generated_fol_theorems :
	rm -r -f $(output_dir)
	make generated_fol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-FOL theorems

flags_generated_non_fol_theorems = \
  -i$(non_fol_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)/$(non_fol_theorems_path)

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
	    $(APIA) -v 0\
	            $(flags_generated_non_fol_theorems) \
	            --schematic-propositional-functions \
	            $*.agda \
            ;; \
          "${non_fol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) -v 0\
	            $(flags_generated_non_fol_theorems) \
	            --schematic-propositional-symbols \
	            $*.agda \
            ;; \
          *) \
	    exit 1 \
            ;; \
        esac
	@diff -r $* $(output_dir)/$*

generated_non_fol_theorems_aux : \
  $(generated_non_fol_theorems_files)

generated_non_fol_theorems :
	rm -r -f $(output_dir)
	make generated_non_fol_theorems_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Generated non-theorems

flags_genetared-non-theorems = \
  -i$(non_theorems_path) \
  --only-files \
  --output-dir=$(output_dir)/$(non_theorems_path)

%.generated_non_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@$(APIA) -v 0 $(flags_genetared-non-theorems) $*.agda
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
	make generated_non_fol_theorems
	make generated_non_theorems
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only FOL theorems files

%.only_fol_theorems :
	$(AGDA) -i$(fol_theorems_path) $*.agda
	$(APIA) -i$(fol_theorems_path) \
                --only-files \
                --output-dir=$(output_dir) \
                $*.agda

only_fol_theorems : $(only_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only non-FOL

%.only_non_fol_theorems :
	$(AGDA) -i$(non_fol_theorems_path) $*.agda
	case $*.agda in \
          "${non_fol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
          "${non_fol_theorems_path}/Eta-Issue8.agda" | \
          "${non_fol_theorems_path}/Existential.agda" | \
          "${non_fol_theorems_path}/Instance.agda" | \
          "${non_fol_theorems_path}/Issue12.agda" | \
          "${non_fol_theorems_path}/OptionsLList.agda" | \
          "${non_fol_theorems_path}/P11.agda" | \
          "${non_fol_theorems_path}/PropositionalFunction.agda") \
	    $(APIA) -i$(non_fol_theorems_path) \
	            --only-files \
	            --output-dir=$(output_dir) \
	            --schematic-propositional-functions \
                    $*.agda \
            ;; \
          "${non_fol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) -i$(non_fol_theorems_path) \
	            --only-files \
	            --output-dir=$(output_dir) \
	            --schematic-propositional-symbols \
                    $*.agda \
            ;; \
          *) \
	    exit 1 \
            ;; \
        esac

only_non_fol_theorems : \
  $(only_non_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove FOL theorems

%.prove_fol_theorems :
	$(AGDA) -i$(fol_theorems_path) $*.agda
	$(APIA) -i$(fol_theorems_path) \
	        --output-dir=$(output_dir) \
	        --time=10 \
                $*.agda

prove_fol_theorems : $(prove_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove non-FOL theorems

%.prove_non_fol_theorems :
	$(AGDA) -i$(non_fol_theorems_path) $*.agda
	case $*.agda in \
          "${non_fol_theorems_path}/AgdaInternalTerms/VarEmptyArgumentsTerm.agda" | \
          "${non_fol_theorems_path}/Eta-Issue8.agda" | \
          "${non_fol_theorems_path}/Existential.agda" | \
          "${non_fol_theorems_path}/Instance.agda" | \
          "${non_fol_theorems_path}/Issue12.agda" | \
          "${non_fol_theorems_path}/OptionsLList.agda" | \
          "${non_fol_theorems_path}/P11.agda" | \
          "${non_fol_theorems_path}/PropositionalFunction.agda") \
	    $(APIA) -i$(non_fol_theorems_path) \
	            --output-dir=$(output_dir) \
	            --time=10 \
                    --schematic-propositional-functions \
		    $*.agda \
            ;; \
          "${non_fol_theorems_path}/PropositionalSymbol.agda") \
	    $(APIA) -i$(non_fol_theorems_path) \
	            --output-dir=$(output_dir) \
	            --time=10 \
                    --schematic-propositional-symbols \
		    $*.agda \
            ;; \
          *) \
	    exit 1 \
            ;; \
        esac

prove_non_fol_theorems : \
  $(prove_non_fol_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove all theorems

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

refute_theorems : $(refute_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Command-line options

%.command_line_options :
	@$(AGDA) -i$(command_line_options_path) $*.agda

# Tested with shelltestrunner 1.3.5.
command_line_options : $(command_line_options_files)
	shelltest --color --precise \
                  $(command_line_options_path)/command-line-options.test
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

haddock_file = /tmp/haddock.tmp

doc :
	cabal configure
	cabal haddock --executables \
	              --haddock-option=--use-unicode \
	              --hyperlink-source > $(haddock_file)
	cat $(haddock_file)
	diff <(find src/ -name '*.hs' | wc -l) \
	     <(grep 100% $(haddock_file) | wc -l)
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
	$(APIA) $(prove_notes_path) \
	        --output-dir=$(output_dir) \
	        --time=10 \
	        $*.agda

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
	cabal install --only-dependencies
# For the use of `-fforce-recomp` see
# https://github.com/haskell/cabal/issues/1844.
	cabal configure --ghc-option=-fforce-recomp
	cabal build
	make generated_all
	make errors
	make command_line_options
	make prove_notes
	@echo "$@ succeeded!"

##############################################################################
# Hlint test

hlint :
	hlint --color=never \
              --cpp-file=dist/build/autogen/cabal_macros.h \
              --cpp-include=src/Apia/ \
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
# For the use of `-fforce-recomp` see
# https://github.com/haskell/cabal/issues/1844.
	cabal install --disable-documentation --ghc-option=-fforce-recomp

##############################################################################
# Haskell program coverage

# TODO: Fix

hpc_html_dir = $(apia_path)/hpc

hpc : hpc_clean
	cd $(apia_path) && cabal clean && cabal install --ghc-option=-fhpc
	make prove_all_theorem
	make refute_theorems
	make errors
	make command_line_options
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
	find -name '*.agdai' | xargs rm -f
	find -name '*.hi' | xargs rm -f
	find -name '*.o' | xargs rm -f
	find -name 'apia.tix' | xargs rm -f
	find -name 'model' | xargs rm -f
	rm -f -r $(output_dir)
