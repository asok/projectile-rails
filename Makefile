all: test

unit-test:
	cask exec ./test/run-tests

integration-test:
	cask exec ecukes

test: unit-test integration-test

.PHONY: test
