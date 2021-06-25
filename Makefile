all: test

unit-test:
	cask exec ./test/run-tests

integration-test:
	cask exec ecukes --quiet --tags ~@pending
ci-integration-test:
	cask exec ecukes --quiet --tags ~@pending,~@no_ci

test: unit-test integration-test
ci-test: unit-test ci-integration-test

.PHONY: test ci-test
