# Makefile
# $Id: Makefile,v 1.2 2003/11/18 22:51:09 berke Exp $ 

include Makefile.config

OCAMLC    = $(CAML)c.opt -g -thread
OCAMLOPT  = $(CAML)opt.opt -thread
#OCAMLOPT  = $(CAML)opt -unsafe -inline 9
#OCAMLOPT  = $(CAML)opt.opt -unsafe -inline 9
OCAMLDEP  = $(CAML)dep

MLINCDIRS = 

EXEC      = tetris
MODULES   = human game tetris play

LIBRARIES = unix threads graphics
CLIBS     =

CMO_OBJS  = $(addsuffix .cmo, $(MODULES))
CMX_OBJS  = $(addsuffix .cmx, $(MODULES))
CMA_OBJS  = $(addsuffix .cma, $(LIBRARIES))
CMXA_OBJS = $(addsuffix .cmxa, $(LIBRARIES))

BYTE_OBJS = $(CMA_OBJS) $(CMO_OBJS)
OPT_OBJS  = $(CMXA_OBJS) $(CMX_OBJS)

INCLUDES  = $(addprefix -I , $(MLINCDIRS))
LINK_OPTS = $(addprefix -ccopt -L, $(CLIBDIRS)) \
	    $(addprefix -cclib -l, $(CLIBS))
#	    -cclib "$(WITH_X)"

byte: $(EXEC)
opt: $(EXEC).opt 
allopt: opt
all: byte opt

$(EXEC): $(CMO_OBJS)
	$(OCAMLC) -custom $(INCLUDES) $(BYTE_OBJS) $(LINK_OPTS) -o $(EXEC)

$(EXEC).opt: $(CMX_OBJS)
	$(OCAMLOPT) $(INCLUDES) $(OPT_OBJS) $(LINK_OPTS) -o $(EXEC).opt

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) $(INCLUDES) -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -c $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c $<

count:
	wc -l *.ml *.mli | sort

clean:
	rm -f *.cm[oix] *.o $(EXEC) $(EXEC).opt *~ .depend

.depend: *.mli *.ml
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

