OCAML=ocamlfind ocamlc -linkpkg -thread -package core
INCLUDE=-I ../include
OCAML_FLAGS=$(INCLUDE)
HEADERS=
OBJS=semant.cmo codegen.cmo
TARGETS=$(HEADERS) $(OBJS)

%: %.ml $(OBJS)
	$(OCAML) $(OCAML_FLAGS) -o $@ $(LIBS) $(OBJS) $<

all: $(TARGETS)

%.cmo: %.ml
	ocamlc $(OCAML_FLAGS) -c $<
%.cmi: %.mli
	$(OCAML) -c $(OCAML_FLAGS) -o $@ $<

clean:
	rm -rf *.cmo *.cmi
