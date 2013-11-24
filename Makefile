SHELL=/bin/bash -O extglob -c

all: test

unit-test:
	cask exec ./test/run-tests

#when run in default order some of the scenarios fail with "OpenStep is not in use or not initialized" error
integration-test:
	cask exec ecukes features/!(rake.feature|generate.feature) features/rake.feature features/generate.feature

test: unit-test integration-test

.PHONY: test
