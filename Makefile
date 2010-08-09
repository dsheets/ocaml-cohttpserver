.DEFAULT: all
all:
	@cd server && $(MAKE)
	@cd htmlgen && $(MAKE)
	@cd crunch && $(MAKE)

%:
	@cd server && $(MAKE) $*
	@cd htmlgen && $(MAKE) $*
	@cd crunch && $(MAKE) $*

