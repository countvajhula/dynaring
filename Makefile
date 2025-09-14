# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))
# Makefile for dynaring

EMACS=emacs

# For a single-package repo, CI_PROJECT is unset.
# CI_PACKAGES contains the single package name.
export CI_PACKAGES=dynaring

help:
	@echo "Run common development actions."
	@echo
	@echo "setup-ci            - Clone the emacs-ci repo."
	@echo "bootstrap           - Install Straight.el."
	@echo "install             - Install package dependencies."
	@echo "build               - Byte compile the package."
	@echo "lint                - Check style with package-lint."
	@echo "checkdoc            - Check docstrings."
	@echo "test                - Run ERT tests."
	@echo "coverage-local      - Run tests and generate a local coverage report."
	@echo "coverage-coveralls  - Run tests and report coverage to Coveralls (for CI)."

setup-ci:
	git clone -b add-coverage-module https://github.com/countvajhula/emacs-ci.git

clean:
	cd emacs-ci && rm -rf ci-init

bootstrap:
	cd emacs-ci && emacs --batch --quick --load bootstrap.el

install:
	cd emacs-ci && emacs --batch --quick --load install.el

build:
	cd emacs-ci && emacs --batch --quick --load build.el

lint:
	cd emacs-ci && emacs --batch --quick --load lint.el

checkdoc:
	cd emacs-ci && emacs --batch --quick --load checkdoc.el

test: build
	cd emacs-ci && emacs --batch --quick --load test.el

coverage-local: install
	# Ensure the target directory exists at the project root.
	mkdir -p coverage
	# Export the variables to ensure they are reliably passed to the subprocess.
	# Force undercover to run
	export UNDERCOVER_FORCE=true && \
	export UNDERCOVER_CONFIG='("*.el" (:report-file "coverage/local-report.json") (:report-format text) (:send-report nil))' && \
	cd emacs-ci && emacs --batch --quick --load coverage.el

coverage-coveralls: install
	cd emacs-ci && emacs --batch --quick --load coverage.el

.PHONY: help setup-ci clean bootstrap install build lint checkdoc test coverage-local coverage-coveralls
