# Shell functions for running R scripts
# Simplified version - runs R directly on local machine

R = Rscript

# If 'make -n' option is invoked, just print the command
ifneq (,$(findstring n,$(MAKEFLAGS)))
R := @echo R
endif
