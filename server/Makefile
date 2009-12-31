OCAMLMAKEFILE=OCamlMakefile

SOURCES = http_tcp_server.ml http_daemon.ml magic_mime.ml
PACKS = lwt cohttp
RESULT = cohttpserver
LIB_PACK_NAME = cohttpserver
ANNOTATE = yes

.PHONY: all
all: pack-byte-code pack-native-code cohttpserver.cma cohttpserver.cmxa
	@ :

DISTVERSION = 0.1

META: META.in
	cat META.in | sed -e 's/@DISTVERSION@/$(DISTVERSION)/' > META

LIBINSTALL_FILES = META cohttpserver.cma cohttpserver.cmxa cohttpserver.a cohttpserver.cmi

install: libinstall
uninstall: libuninstall
reinstall: uninstall install

-include $(OCAMLMAKEFILE)
