SHELL := /bin/bash

# Allow overriding the uv binary if needed (e.g., UV=uvx)
UV ?= uv
UV_RUN := $(UV) run

# Tool shims that always run inside the project env
PYTHON    := $(UV_RUN) python
COVERAGE  := $(UV_RUN) coverage
MYPY      := $(UV_RUN) mypy
PRECOMMIT := $(UV_RUN) pre-commit

ARGS ?=

.PHONY: init lint type test cov check ch bump test-emacs update-emacs thaw-emacs clean help

##@ Development

init:  ## Install pre-commit hooks
	$(PRECOMMIT) install

lint:  ## Lint codebase with pre-commit
	$(PRECOMMIT) run --all-files --show-diff-on-failure $(ARGS)

type:  ## Type-check Python scripts with mypy
	$(MYPY) scripts/

##@ Testing

test:  ## Run tests with pytest and coverage
	$(COVERAGE) run -p -m pytest -v

cov:  ## Generate coverage report
	$(COVERAGE) combine
	$(COVERAGE) report
	$(COVERAGE) xml

##@ Quality

check: lint type test cov  ## Run all checks (lint + type + test + cov)

##@ Release

ch:  ## Update CHANGELOG.md with unreleased changes
	set -euo pipefail; \
	git cliff --bump --unreleased -o RELEASE.md; \
	$(PYTHON) scripts/update_changelog.py --raw RELEASE.md --changelog CHANGELOG.md; \
	rm -f RELEASE.md; \
	echo "CHANGELOG.md updated."

bump:  ## Bump version, update changelog, and tag release
	set -euo pipefail; \
	NEXT_VERSION=$$(git cliff --bumped-version); \
	echo "Bumping to $$NEXT_VERSION"; \
	$(UV) version "$$NEXT_VERSION"; \
	$(UV) lock; \
	$(UV) sync --locked --all-groups; \
	$(MAKE) ch; \
	if ! git diff --quiet; then git add -A && git commit -m "chore: release $$NEXT_VERSION"; else echo "No changes to commit"; fi; \
	git tag -a "$$NEXT_VERSION" -m "Release $$NEXT_VERSION"

##@ Emacs

# Advice to auto-resolve interactive prompts in batch mode:
# "y" (yes/confirm) → accept, "c" (cancel/skip) → skip, else quit.
STRAIGHT_BATCH_ADVICE := (advice-add (quote straight--popup-raw) :override \
  (lambda (msg actions) (message "BATCH: %s" msg) \
    (let ((yes (assoc "y" actions)) \
          (cancel (or (assoc "c" actions) (assoc "C-g" actions)))) \
      (cond (yes (funcall (nth 2 yes))) \
            (cancel (funcall (nth 2 cancel))) \
            (t (signal (quote quit) (list msg)))))))
EMACS_BATCH = emacs --batch -l ~/.emacs.d/init.el

test-emacs:  ## Run Emacs smoke tests
	$(EMACS_BATCH) -l ~/.emacs.d/test/test.el

update-emacs:  ## Pull, normalize, freeze, thaw, check packages
	$(EMACS_BATCH) --eval '(progn $(STRAIGHT_BATCH_ADVICE) (straight-pull-all) (straight-normalize-all) (straight-freeze-versions) (straight-thaw-versions) (straight-check-all))'

thaw-emacs:  ## Restore straight.el repos to match lockfile
	$(EMACS_BATCH) --eval '(progn $(STRAIGHT_BATCH_ADVICE) (straight-thaw-versions) (straight-check-all))'

##@ Maintenance

clean:  ## Remove temporary and cache files
	rm -rf ./__pycache__ ./.mypy_cache ./.pytest_cache ./.ruff_cache .coverage

##@ Help

help:  ## Show this help
	@echo "Available targets:"
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
