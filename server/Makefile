OCAMLMAKEFILE=../OCamlMakefile

SOURCES = http_tcp_server.ml http_daemon.ml mime_types.ml magic_mime.ml
PACKS = lwt cohttp
RESULT = cohttpserver
LIB_PACK_NAME = cohttpserver
ANNOTATE = yes

.PHONY: all
all: pack-byte-code pack-native-code cohttpserver.cma cohttpserver.cmxa
	@ :

.PHONY: depend
depend: mime_types.ml
	@ :
	
DISTVERSION = 0.1

META: META.in
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

LIBINSTALL_FILES = META cohttpserver.cma cohttpserver.cmxa cohttpserver.a cohttpserver.cmi

mime_types.ml: mime.types generate_mime_types.ml
	ocaml str.cma generate_mime_types.ml $< > $@

install: libinstall
uninstall: libuninstall
reinstall: uninstall install

-include $(OCAMLMAKEFILE)
