.DEFAULT: all
all:
	@cd server && $(MAKE)
	@cd htmlgen && $(MAKE)

%:
	@cd server && $(MAKE) $*
	@cd htmlgen && $(MAKE) $*

