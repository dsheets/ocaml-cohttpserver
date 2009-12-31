OCAMLMAKEFILE=../OCamlMakefile

SOURCES = enum.ml markdown.ml markdown_html.ml
RESULT = htmlgen
LIB_PACK_NAME = htmlgen
ANNOTATE = yes

.PHONY: all
all: pack-byte-code pack-native-code 
	@ :

DISTVERSION = 0.1

.PHONY: depend
depend: META
	@ :

META: META.in Makefile
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

LIBINSTALL_FILES = META htmlgen.cma htmlgen.cmxa

install: libinstall
uninstall: libuninstall
reinstall: uninstall install

-include $(OCAMLMAKEFILE)
