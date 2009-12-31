OCAMLMAKEFILE=../OCamlMakefile

BINDIR ?= /usr/local/bin

SOURCES = custom_unix_stubs.c crunch.ml
RESULT = mlcrunch
PACKS = unix

.PHONY: all install
all: nc
	@ :

install:
	mkdir -p $(DESTDIR)$(BINDIR)
	cp $(RESULT) $(DESTDIR)$(BINDIR)/mlcrunch
	chmod 755 $(DESTDIR)$(BINDIR)/mlcrunch

reinstall: install
	@ :

-include $(OCAMLMAKEFILE)
