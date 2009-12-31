.PHONY: all clean
all:
	@cd server && $(MAKE) all

clean:
	@cd server && $(MAKE) clean
