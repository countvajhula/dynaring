# Makefile for dynaring

EMACS=emacs

# For a single-package repo, CI_PROJECT is unset.
# CI_PACKAGES contains the single package name.
export CI_PACKAGES=dynaring

help:
	@echo "Run common development actions."
	@echo
	@echo "setup-ci   - Clone the emacs-ci repo."
	@echo "bootstrap  - Install Straight.el."
	@echo "install    - Install package dependencies."
	@echo "build      - Byte compile the package."
	@echo "lint       - Check style with package-lint."
	@echo "checkdoc   - Check docstrings."
	@echo "test       - Run ERT tests."

setup-ci:
	git clone https://github.com/countvajhula/emacs-ci.git

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

.PHONY: help setup-ci clean bootstrap install build lint checkdoc test
