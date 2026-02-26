# Usage

## Development

The project uses [uv](https://docs.astral.sh/uv/) for Python dependency
management and [direnv](https://direnv.net/) for automatic environment
activation.

### Make Targets

Run `make help` for the full list:

```text
  init            Install pre-commit hooks
  lint            Lint codebase with pre-commit
  type            Type-check Python scripts with mypy
  test            Run tests with pytest and coverage
  xdoc            Run xdoctest on scripts
  check           Run all checks (lint + type + test + cov)
  docs            Build Sphinx documentation
  bump            Bump version, update changelog, and tag release
  test-emacs      Run Emacs smoke tests
  upgrade-emacs   Full upgrade: update + smoke tests
  thaw-emacs      Restore straight.el repos to match lockfile
  clean           Remove temporary and cache files
```

### Commit Conventions

Follow [Conventional Commits](https://www.conventionalcommits.org/). Scopes
reflect the configuration domain:

```text
feat(emacs): add consult-ripgrep binding
fix(git): correct delta pager config
chore(deps): update uv.lock
```

### Testing

```bash
make test                # Python script tests
make test-emacs          # Emacs smoke tests (~70 checks)
make xdoc                # Run doctests
make check               # lint + type + test + coverage
```

## CI / Automation

| Workflow              | Trigger          | Purpose                                 |
| --------------------- | ---------------- | --------------------------------------- |
| `ci.yml`              | push, PR         | pre-commit lint, pytest, auto-merge     |
| `update-emacs.yml`    | dispatch, push   | Emacs smoke tests / package update + PR |
| `cruft-update.yml`    | weekly, dispatch | Apply cookiecutter template updates     |
| `lockfile-update.yml` | weekly, dispatch | `uv lock --upgrade` + PR                |
| `release.yml`         | tag push         | Publish release                         |
