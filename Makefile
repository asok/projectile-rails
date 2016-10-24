all: test

unit-test:
	cask exec ./test/run-tests

integration-test:
	cask exec ecukes --quiet

test: unit-test integration-test

.PHONY: test
