.PHONY: backend frontend includes clean_backend clean_frontent clean_includes
TARGETS=includes backend frontend
CLEAN=clean_frontend clean_backend clean_includes

all: $(TARGETS)

frontend:
	make -C $@
backend:
	make -C $@
includes:
	make -C include

clean_frontend:
	make -C frontend clean
clean_backend:
	make -C backend clean
clean_includes:
	make -C include clean
clean: $(CLEAN)
