SHELL := /bin/bash

##############################################################################
# Paths

# Tests paths.
errors_path               = test/errors
non_theorems_path         = test/non-theorems
command_line_options_path = test/command-line-options
theorems_path             = test/theorems

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

generated_theorems_files = \
  $(call my_pathsubst,generated_theorems,$(theorems_path))

generated_non_theorems_files = \
  $(call my_pathsubst,generated_non_theorems,$(non_theorems_path))

only_theorems_files = $(call my_pathsubst,only_theorems,$(theorems_path))

prove_theorems_files = $(call my_pathsubst,prove_theorems,$(theorems_path))

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
# Test suite: Generated conjectures

flags_gt = -i$(theorems_path) --only-files \
	   --output-dir=$(output_dir)/$(theorems_path) \

%.generated_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(theorems_path) $*.agda
	@$(APIA) -v 0 $(flags_gt) $*.agda
	@diff -r $* $(output_dir)/$*

flags_ngt = -i$(non_theorems_path) --only-files \
	   --output-dir=$(output_dir)/$(non_theorems_path) \

%.generated_non_theorems :
	@echo "Comparing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@$(APIA) -v 0 $(flags_ngt) $*.agda
	@diff -r $* $(output_dir)/$*

generated_conjectures_aux : $(generated_theorems_files) \
	                    $(generated_non_theorems_files)

generated_conjectures :
	rm -r -f $(output_dir)
	make generated_conjectures_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Only files

%.only_theorems :
	$(AGDA) -i$(theorems_path) $*.agda
	$(APIA) -i$(theorems_path) --only-files --output-dir=$(output_dir) \
                $*.agda

only_theorems : $(only_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove theorems

%.prove_theorems :
	$(AGDA) -i$(theorems_path) $*.agda
	$(APIA) -i$(theorems_path) --output-dir=$(output_dir) \
	         --time=10 $*.agda

prove_theorems : $(prove_theorems_files)
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

# Tested with shelltestrunner 1.3.4 compiled with GHC 7.8.3.
command_line_options : $(command_line_options_files)
	shelltest --color --precise \
                  $(command_line_options_path)/command-line-options.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Error messages

%.errors :
	@$(AGDA) -i$(errors_path) $*.agda

# Tested with shelltestrunner 1.3.4 compiled with GHC 7.8.3.
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
	$(APIA) $(prove_notes_path) --output-dir=$(output_dir) --time=10 $*.agda

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
# cabal clean should be unnecessary. See issue
# https://github.com/haskell/cabal/issues/1844
	cabal clean
	cabal install --only-dependencies
	cabal configure
	cabal build
	make generated_conjectures
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

install :
# cabal clean should be unnecessary. See issue
# https://github.com/haskell/cabal/issues/1844
	cabal clean
	cabal install --disable-documentation

##############################################################################
# Haskell program coverage

# TODO: Fix

hpc_html_dir = $(apia_path)/hpc

hpc : hpc_clean
	cd $(apia_path) && cabal clean && cabal install --ghc-option=-fhpc
	make prove_theorems
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
	hasktags -e $(haskell_files)

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
