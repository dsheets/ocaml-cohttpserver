.DEFAULT: all
all:
	@cd server && $(MAKE)
	@cd lib && $(MAKE)

%:
	@cd server && $(MAKE) $*
	@cd lib && $(MAKE) $*

