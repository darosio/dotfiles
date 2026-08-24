SHELL := /bin/bash

# Allow overriding the uv binary if needed (e.g., UV=uvx)
UV ?= uv
UV_RUN := $(UV) run

# Tool shims that always run inside the project env
PYTHON    := $(UV_RUN) python
COVERAGE  := $(UV_RUN) coverage
MYPY      := $(UV_RUN) mypy
PRECOMMIT := $(UV_RUN) pre-commit
SPHINXBUILD := $(UV_RUN) sphinx-build
XDOCTEST    := $(UV_RUN) python -m xdoctest

# SPHINXOPTS ?= -W
SPHINXOPTS ?=
DOCS_SRC   := docs
DOCS_OUT   := docs/_build
ARGS ?=

.PHONY: init lint type test cov check ch bump test-emacs update-emacs upgrade-emacs thaw-emacs clean help docs docs-clean docs-serve


# Documentation
docs:  ## Build docs
	$(SPHINXBUILD) $(SPHINXOPTS) $(DOCS_SRC) $(DOCS_OUT)

docs-clean:  ## Cleans the documentation build directory
	rm -rf $(DOCS_OUT)

docs-serve:  ## Serves the documentation locally
	$(PYTHON) -m http.server 8000 -d $(DOCS_OUT)


# Development setup
init:  ## Installs pre-commit hooks for version control.
	$(PRECOMMIT) install


# Code quality
lint:  ## Lints the codebase using pre-commit.
	$(PRECOMMIT) run --all-files --show-diff-on-failure $(ARGS)


# Testing
test:  ## Runs tests using pytest and coverage
	$(COVERAGE) run -p -m pytest -v

cov:  ## Generates a coverage report in multiple formats (report, xml).
	$(COVERAGE) combine
	$(COVERAGE) report
	$(COVERAGE) xml

type:  ## Checks the type annotations of Python files using mypy.
	$(MYPY) src/.local/bin tests

xdoc:  ## Runs xdoctest on the project.
	$(XDOCTEST) src/.local/bin/mm_organize.py all

check: lint type test cov  ## Run all checks (lint + type + test + cov)


# Release management
ch:  ## Updates CHANGELOG.md with condensed release notes.
	set -euo pipefail; \
	git cliff --bump --unreleased -o RELEASE.md; \
	$(UV) run python scripts/update_changelog.py --raw RELEASE.md --changelog CHANGELOG.md; \
	rm -f RELEASE.md; \
	echo "CHANGELOG.md updated."

bump:  ## Bumps version, updates changelog, commits and tags.
	@if ! git diff --quiet || ! git diff --cached --quiet; then \
		echo "Error: working tree is dirty. Commit or stash changes first." >&2; exit 1; \
	fi
	set -euo pipefail; \
	NEXT_VERSION=$$(git cliff --bumped-version); \
	echo "Bumping to $$NEXT_VERSION"; \
	$(UV) version "$$NEXT_VERSION"; \
	$(UV) lock; \
	$(MAKE) ch; \
	git add -u && git commit -m "chore: release $$NEXT_VERSION"; \
	git tag -a "$$NEXT_VERSION" -m "Release $$NEXT_VERSION"
	# git push; \
	# git push --tags

# Emacs

# Advice to auto-resolve interactive prompts in batch mode:
# - accept ordinary yes/confirm prompts
# - discard dirty worktrees in straight repos so vendor caches don't block upgrades
# - refuse to write lockfiles when some repos still have unpushed local commits
# - otherwise cancel/skip when possible
STRAIGHT_BATCH_ADVICE := (advice-add (quote straight--popup-raw) :override \
  (lambda (msg actions) (message "BATCH: %s" msg) \
    (let ((yes (assoc "y" actions)) \
          (no (assoc "n" actions)) \
          (discard (assoc "d" actions)) \
          (cancel (or (assoc "c" actions) (assoc "C-g" actions)))) \
      (cond ((and (string-match-p "Really write lockfiles[?]" msg) no) \
             (funcall (nth 2 no))) \
            ((and (string-match-p "Really write lockfiles[?]" msg) cancel) \
             (funcall (nth 2 cancel))) \
            ((and (string-match-p "dirty worktree" msg) discard) \
             (funcall (nth 2 discard))) \
            (yes (funcall (nth 2 yes))) \
            (discard (funcall (nth 2 discard))) \
            (cancel (funcall (nth 2 cancel))) \
            (t (signal (quote quit) (list msg)))))))
# If the build cache is missing/empty (or was wiped/corrupted), straight
# thinks nothing changed and skips rebuilding, leaving stale byte-compiled
# packages that reference old macros (e.g. compat's static-unless). Force a
# full rebuild whenever that happens.
STRAIGHT_CACHE_CHECK := (let ((cache (expand-file-name "straight/build-cache.el" user-emacs-directory))) \
  (when (or (not (file-exists-p cache)) (= 0 (nth 7 (file-attributes cache)))) \
    (message "BATCH: straight build cache missing or empty; forcing full rebuild") \
    (straight-rebuild-all)))
EMACS_BATCH = emacs --batch -l ~/.emacs.d/init.el
# Bootstrap-only invocation: loads straight.el but not the rest of init.el, so
# a broken package config (e.g. from a version bump) can't crash before
# thaw/update/freeze maintenance even gets a chance to run and fix it.
EMACS_BATCH_BOOTSTRAP = emacs --batch --eval '(progn (defvar bootstrap-version 7) (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))) (load bootstrap-file nil (quote nomessage))))'

test-emacs:  ## Run Emacs smoke tests
	$(EMACS_BATCH) -l ~/.emacs.d/test/test.el

update-emacs:  ## Thaw, pull, normalize, and check straight.el packages
	$(EMACS_BATCH_BOOTSTRAP) --eval '(progn $(STRAIGHT_BATCH_ADVICE) (straight-thaw-versions) (straight-pull-all) (straight-normalize-all) $(STRAIGHT_CACHE_CHECK) (straight-check-all))'

upgrade-emacs: update-emacs test-emacs  ## Full upgrade: update, smoke tests, freeze pins
	$(EMACS_BATCH_BOOTSTRAP) --eval '(progn $(STRAIGHT_BATCH_ADVICE) (straight-freeze-versions))'

thaw-emacs:  ## Restore straight.el repos to match lockfile
	$(EMACS_BATCH_BOOTSTRAP) --eval '(progn $(STRAIGHT_BATCH_ADVICE) (straight-thaw-versions) $(STRAIGHT_CACHE_CHECK) (straight-check-all))'


# Project cleanup
clean:  ## Project cleanup
	rm -rf ./build .coverage ./__pycache__ ./.mypy_cache ./.ruff_cache ./.pytest_cache ./docs/_build ./tests/__pycache__ ./dist ./src/{{ cookiecutter.project_slug }}/__pycache__


# Help target to show all available commands
help: ## Show this help message.
	@echo "Available targets:"
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
