include ../master.Makefile

EXAMPLES=configwin_example.x configwin_example2.x okey_example.x

all: $(EXAMPLES)

clean:
	rm -f *.cm*
	rm $(EXAMPLES)

.SUFFIXES: .ml .x
%.x: %.ml
	$(OCAMLFIND) ocamlc -package lablgtk2-extras.configwin -linkpkg -o $@ $<

