OPAMBIN   := $(shell opam config var bin)
OCAMLOPT  := $(OPAMBIN)/ocamlopt.opt
OCAMLDEP  := $(OPAMBIN)/ocamldep
OCAMLLEX  := $(OPAMBIN)/ocamllex
OCAMLYACC := $(OPAMBIN)/ocamlyacc
CC        := gcc

# libraries
OPAMDIR    := $(shell opam config var lib)
APRONDIR   := $(OPAMDIR)/apron
GMPDIR     := $(OPAMDIR)/gmp
ZARITHDIR  := $(OPAMDIR)/zarith
OCAMLDIR   := $(OPAMDIR)/ocaml

#ocaml libraries
LIBS         := bigarray gmp zarith apron polkaMPQ octD boxMPQ \
                str unix graphics
OCAMLOPTLIBS := $(LIBS:%=%.cmxa)

#sources directories
SOURCESINC := -I src -I src/domains -I src/frontend -I src/print -I src/solver

# directories to include : sources + lib
OCAMLINC  := -I $(APRONDIR) -I $(GMPDIR) -I $(ZARITHDIR) \
             -I src -I src/lib -I src/domains -I src/frontend -I src/print \
						 -I src/solver

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
	src/lib/mapext.ml \
	src/lib/bound_sig.ml \
	src/lib/bound_sig_simple.ml \
	src/lib/bound_float.ml \
	src/lib/trigo.ml \
	src/lib/itv_sig.ml \
	src/lib/itv_simple.ml \
	src/lib/itv.ml \
	src/frontend/csp.ml \
	src/frontend/rewrite.ml \
	src/frontend/parser.ml \
	src/frontend/lexer.ml \
	src/frontend/builder.ml \
  src/domains/apron_domain.ml \
  src/domains/cartesian.ml \
  src/domains/domain_signature.ml \
  src/domains/domain_signature2.ml	\
  src/domains/relational.ml \
  src/solver/result.ml \
  src/solver/splitter.ml \
  src/solver/solver.ml \
  src/print/view.ml \
  src/print/objgen.ml \
	src/print/latex.ml \
  src/print/drawer_sig.ml \
	src/print/box_drawer.ml \
	src/print/apron_drawer.ml \
  src/print/out.ml


# targets
TARGETS = solver.opt

# mains
ABS   = src/main.ml
CHECK = src/solver/checker.ml \
				src/check.ml

# object files
CMIFILES = $(MLIFILES:%.ml=%.cmi)
CMXFILES = $(MLFILES:%.ml=%.cmx)

# c files
CFILES = src/lib/ml_float.c
OFILES = $(CFILES:%.c=%.o)

# rules
all: $(TARGETS)
	@mkdir -p out

solver.opt: $(OFILES) $(CMXFILES) $(ABS)
	$(OCAMLOPT) -o $@ $(OCAMLINC) $(OCAMLOPTLIBS) $+

check: checker.opt
	@./checker.opt
	@rm checker.opt

checker.opt: $(OFILES) $(CMXFILES) $(CHECK)
	@$(OCAMLOPT) -o $@ $(OCAMLINC) $(OCAMLOPTLIBS) $+

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $*.ml

%.cmi: %.mli %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

%.o: %.c
	$(CC) -I $(OCAMLDIR) -o $@ -c $+

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

clean:
	rm -f .depend $(TARGETS) $(AUTOGEN)
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "*.o"`
	rm -f out/*
	rm -f -R out

MLSOURCES = $(MLFILES) $(ABS) $(CHECK) $(MLIFILES)

.depend: $(MLSOURCES) Makefile
	@-$(OCAMLDEP) $(OCAMLINC) -native $(MLSOURCES) > .depend

.phony:	all clean

include .depend
