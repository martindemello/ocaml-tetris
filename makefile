export PP = camlp4o

PPFLAGS = \
	-I `ocamlfind -query batteries.pa_comprehension` pa_comprehension.cmo \
	-I `ocamlfind -query batteries.pa_batteries` pa_batteries.cmo \
	-I `ocamlfind -query batteries.pa_strings` pa_strings.cma \
	-I `ocamlfind -query batteries.pa_where` pa_where.cmo

RESULT = tetris
SOURCES = human.ml tetris.ml

PACKS = allegro batteries
CREATE_LIB = yes

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
