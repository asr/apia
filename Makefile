# Tested with:
# GNU Make 3.81 and
# GNU bash, version 4.2.8(1)-release (x86_64-pc-linux-gnu)

SHELL := /bin/bash

##############################################################################
# Paths

dump-agdai_path     = tools/dump-agdai
fix-whitespace_path = tools/fix-whitespace

# Agda standard library path.
std_lib_path = ~/agda-upstream/std-lib

# Tests paths
errors_path        = test/errors
non_theorems_path  = test/non-theorems
options_path       = test/options
theorems_path      = test/theorems

# Directory for the TPTP files.
output_dir = /tmp/agda2atp

##############################################################################
# Variables

agda2atp_haskell_files = $(shell find src/ -name '*.hs')

AGDA = agda -v 0

# The defaults ATPs are e, equinox, and vampire.
AGDA2ATP = dist/build/agda2atp/agda2atp
# AGDA2ATP = dist/build/agda2atp/agda2atp --atp=e
# AGDA2ATP = dist/build/agda2atp/agda2atp --atp=equinox
# AGDA2ATP = dist/build/agda2atp/agda2atp --atp=ileancop
# AGDA2ATP = dist/build/agda2atp/agda2atp --atp=metis
# AGDA2ATP = dist/build/agda2atp/agda2atp --atp=spass
# AGDA2ATP = dist/build/agda2atp/agda2atp --atp=vampire

##############################################################################
# Auxiliary functions

my_pathsubst = $(patsubst %.agda,%.$(1), \
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

prove_theorems_files = $(call my_pathsubst,prove_theorems,$(theorems_path))

refute_theorems_files = $(call my_pathsubst,refute_theorems,$(non_theorems_path))

errors_files = $(call my_pathsubst,errors,$(errors_path))

options_files = $(call my_pathsubst,options,$(options_path))

##############################################################################
# Test suite: Generated conjectures

flags_gt = -i$(theorems_path) --only-files \
	   --output-dir=$(output_dir)/$(theorems_path) \

%.generated_theorems :
	@echo "Processing $*.agda"
	@$(AGDA) -i$(theorems_path) $*.agda
	@$(AGDA2ATP) -v 0 $(flags_gt) $*.agda
	@diff -r $* $(output_dir)/$*

flags_ngt = -i$(non_theorems_path) --only-files \
	   --output-dir=$(output_dir)/$(non_theorems_path) \

%.generated_non_theorems :
	@echo "Processing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@$(AGDA2ATP) -v 0 $(flags_ngt) $*.agda
	@diff -r $* $(output_dir)/$*

generated_conjectures_aux : $(generated_theorems_files) \
	                    $(generated_non_theorems_files)

generated_conjectures :
	rm -r -f $(output_dir)
	make generated_conjectures_aux
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Prove theorems

%.prove_theorems :
	$(AGDA) -i$(theorems_path) $*.agda
	$(AGDA2ATP) -i$(theorems_path) --output-dir=$(output_dir) \
	            --time=10 $*.agda

prove_theorems : $(prove_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Refute theorems

%.refute_theorems :
	@echo "Processing $*.agda"
	@$(AGDA) -i$(non_theorems_path) $*.agda
	@if ( $(AGDA2ATP) -i$(non_theorems_path) \
	                 --output-dir=$(output_dir) --time=5 $*.agda ); then \
	    exit 1; \
	fi

refute_theorems : $(refute_theorems_files)
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Options

%.options :
	@$(AGDA) -i$(options_path) $*.agda

# Tested with shelltest 1.3.1.
options : $(options_files)
	shelltest --color $(options_path)/options.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Error messages

%.errors :
	@$(AGDA) -i$(errors_path) $*.agda

# Tested with shelltest 1.3.1.
errors : $(errors_files)
	shelltest --color $(errors_path)/errors.test
	@echo "$@ succeeded!"

##############################################################################
# Test suite: Haddock test

haddock_file = /tmp/haddock.tmp

doc :
	cabal configure
	cabal haddock --executables \
	              --haddock-option=--use-unicode \
	              --hyperlink-source  > $(haddock_file)
	cat $(haddock_file)
	diff <(find src/ -name '*.hs' | wc -l) \
	     <(grep 100% $(haddock_file) | wc -l)
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to Agda

agda_changed : clean
	make agda2atp_changed
	cd $(dump-agdai_path) && cabal clean && cabal configure && cabal build
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a modification to agda2atp

agda2atp_changed : clean
	cabal clean && cabal configure && cabal build
	@make generated_conjectures
	@make errors
	@make options
	@echo "$@ succeeded!"

##############################################################################
# Test used when there is a new ATP or a new version of an ATP

atp_changed :
	@make generated_conjectures
	@make prove_theorems
	@make refute_theorems
	@make errors
	@make options
	@echo "$@ succeeded!"

##############################################################################
# Hlint test

hlint :
	hlint src/
	hlint $(dump-agdai_path)/src -i "Use on"
	hlint $(fix-whitespace_path)
	@echo "$@ succeeded!"

##############################################################################
# Git : pre-commit test

git_pre_commit :
	@fix-whitespace --check
	@make doc
	@make hlint
	@echo "$@ succeeded!"

##############################################################################
# Install

install :
	cabal install
	cd $(dump-agdai_path) && cabal install
	cd $(fix-whitespace_path) && cabal install

##############################################################################
# Haskell program coverage

# TODO: Fix

hpc_html_dir = $(agda2atp_path)/hpc

hpc : hpc_clean
	cd $(agda2atp_path) && cabal clean && cabal install --ghc-option=-fhpc
	make prove_theorems
	make refute_theorems
	make errors
	make options
	hpc markup --exclude=Paths_agda2atp \
	           --destdir=$(hpc_html_dir) \
	           --srcdir=$(agda2atp_path) \
                   agda2atp.tix
	hpc report --exclude=Paths_agda2atp \
                   --decl-list \
	           --srcdir=$(agda2atp_path) \
                   agda2atp.tix
	rm -f *.tix

hpc_clean :
	rm -f *.tix
	rm -f -r $(hpc_html_dir)

##############################################################################
# Others

.PHONY : TAGS
TAGS :
	hasktags -e $(agda2atp_haskell_files)

TODO :
	find -wholename './dist' -prune -o -print \
	| xargs grep -I 'TODO:' \
	| sort

clean :
	find -name '*.agdai' | xargs rm -f
	find -name '*.hi' | xargs rm -f
	find -name '*.o' | xargs rm -f
	find -name 'agda2atp.tix' | xargs rm -f
	find -name 'model' | xargs rm -f
	rm -f -r $(output_dir)
