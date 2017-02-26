OCAML=ocaml find ocamlc -linkpkg -thread -package core
INCLUDE=-I ../include
OCAML_FLAGS=$(INCLUDE)
HEADERS=
OBJS=
TARGETS=$(HEADERS) $(OBJS) $(CTARGETS) $(BTARGETS)

all: $(TARGETS)

%.cmo: %.ml
	ocamlc $(OCAML_FLAGS) -c $<
%.cmi: %.mli
	$(OCAML) -c $(OCAML_FLAGS) -o $@ $<

clean:
	rm -f $(TARGETS)
