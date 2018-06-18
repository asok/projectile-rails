all: test

unit-test:
	cask exec ./test/run-tests

integration-test:
	cask exec ecukes --quiet --tags ~@pending

test: unit-test integration-test

.PHONY: test
