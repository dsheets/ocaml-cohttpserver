.PHONY: all clean
all:
	@cd server && $(MAKE) all
	@cd lib && $(MAKE) all

clean:
	@cd server && $(MAKE) clean
	@cd lib && $(MAKE) clean
