OCAML=ocaml find ocamlc -linkpkg -thread -package core
OCAML_FLAGS=
TARGETS=

all: $(TARGETS)

%.cmi: %.mli
	$(OCAML) -c $(OCAML_FLAGS) -o $@ $<

clean:
	rm -f $(TARGETS)