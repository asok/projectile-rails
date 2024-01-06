all: test

unit-test:
	cask exec ./test/run-tests

integration-test:
	cask exec ecukes --quiet --tags ~@pending
ci-integration-test:
	cask exec ecukes --quiet --tags ~@pending,~@no_ci

lint:
	cask exec emacs -batch -f package-lint-batch-and-exit projectile-rails.el

test: unit-test integration-test
ci-test: unit-test ci-integration-test

.PHONY: test ci-test
