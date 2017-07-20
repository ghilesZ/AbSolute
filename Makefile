OPAMBIN   := $(shell opam config var bin)
OCAMLC    := $(OPAMBIN)/ocamlc.opt
OCAMLOPT  := $(OPAMBIN)/ocamlopt.opt
OCAMLDEP  := $(OPAMBIN)/ocamldep
OCAMLLEX  := $(OPAMBIN)/ocamllex
OCAMLYACC := $(OPAMBIN)/ocamlyacc
CC        := gcc

# libraries
OPAMDIR   := $(shell opam config var lib)
APRONDIR  := $(OPAMDIR)/apron
GMPDIR    := $(OPAMDIR)/gmp
ZARITHDIR := $(OPAMDIR)/zarith
OCAMLDIR  := $(OPAMDIR)/ocaml

#ocaml libraries
LIBS         := bigarray gmp zarith apron polkaMPQ octD boxMPQ str unix graphics
OCAMLLIBS    := $(LIBS:%=%.cma) $(CCLIB)
OCAMLOPTLIBS := $(LIBS:%=%.cmxa) $(CCLIB)

# directories to include
OCAMLINC  := -I $(APRONDIR) -I $(GMPDIR) -I $(ZARITHDIR) \
             -I src -I src/lib -I src/domains -I src/frontend -I src/print \
             -I src/solver

# targets
TARGETS = solver.opt

AUTOGEN =\
  src/frontend/parser.ml \
  src/frontend/lexer.ml \
  src/frontend/parser.mli \
  src/frontend/modParser.ml \
  src/frontend/modLexer.ml \
  src/frontend/modParser.mli

# source files
MLFILES = \
	src/lib/polynom.ml \
	src/lib/array_maker.ml \
	src/lib/linconsext.ml \
	src/lib/tconsext.ml \
	src/lib/abstractext.ml \
  src/lib/constant.ml \
  src/lib/apron_utils.ml \
  src/lib/bot.ml \
  src/lib/trigo.ml \
  src/lib/mapext.ml \
  src/lib/bound_sig.ml \
  src/lib/bound_sig_simple.ml \
  src/lib/bound_float.ml \
  src/lib/itv_sig.ml \
  src/lib/itv.ml \
  src/lib/itv_simple.ml \
  src/frontend/csp.ml \
	src/frontend/rewrite.ml \
  src/frontend/parser.ml \
  src/frontend/lexer.ml \
  src/frontend/file_parser.ml \
  src/domains/apron_domain.ml \
  src/domains/cartesian.ml \
  src/domains/domain_signature.ml \
  src/domains/relational.ml \
  src/solver/result.ml \
  src/solver/splitter.ml \
  src/solver/solver.ml \
	src/solver/checker.ml \
  src/print/view.ml \
  src/print/objgen.ml \
	src/print/latex.ml \
  src/print/drawer_sig.ml \
	src/print/box_drawer.ml \
	src/print/apron_drawer.ml \
  src/print/out.ml \
	src/main.ml

CFILES = \
  src/lib/ml_float.c

# object files
CMIFILES = $(MLIFILES:%.ml=%.cmi)
CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)
OFILES   = $(CFILES:%.c=%.o)

# rules
all: $(TARGETS)
	@mkdir -p out

solver.opt: $(OFILES) $(CMXFILES)
	$(OCAMLOPT) -o $@ $(OCAMLINC) $(OCAMLOPTLIBS) $+

#minimizer.opt: $(CMXFILES)
#	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $*.ml

%.cmi: %.mli %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

%.o: %.c
	$(CC) -I $(OCAMLDIR) -o $@ -c $+

clean:
	rm -f .depend $(TARGETS) $(AUTOGEN)
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f out/*
	rm -f -R out

MLSOURCES = $(MLFILES) $(MLIFILES)

.depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native $(OCAMLINC) $(MLSOURCES) > .depend

.phony:	all clean

include .depend
