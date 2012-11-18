CABAL ?= cabal

.PHONY: all
all: build

.PHONY: build
build:
	$(CABAL) build

.PHONY: clean
clean:
	$(CABAL) clean

.PHONY: configure
configure:
	$(CABAL) configure

.PHONY: configure-profile
configure-profile:
	$(CABAL) configure --enable-optimization=2 --enable-library-profiling --enable-executable-profiling -fprofile
