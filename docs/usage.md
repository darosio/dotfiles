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

| Workflow              | Trigger          | Purpose                                  |
| --------------------- | ---------------- | ---------------------------------------- |
| `ci.yml`              | push, PR         | pre-commit lint, pytest, auto-merge      |
| `upgrade-emacs.yml`   | dispatch, push   | Emacs smoke tests / package upgrade + PR |
| `cruft-update.yml`    | weekly, dispatch | Apply cookiecutter template updates      |
| `lockfile-update.yml` | weekly, dispatch | `uv lock --upgrade` + PR                 |
| `release.yml`         | tag push         | Publish release                          |

## Git-Annex Modernization

For an old git-annex repository, load the shell configuration and run:

```bash
gamod
```

The helper shows status, runs `git annex sync`, checks content with
`git annex fsck --fast`, upgrades to the newest repository version supported by
the installed git-annex, and enables `annex.addunlocked`, `annex.thin`,
`annex.sshcaching`, and `annex.stalldetection`. It then runs
`git annex find --unlocked` to detect already-unlocked annexed files. If any
are found, it runs `git annex lock .` first, then `git annex unlock .` to
normalize them for thin mode. It applies `chattr -C .git/annex/objects` when
`lsattr` and `chattr` are available.

The helper does not run `git add`, stage files, or commit. Review the result:

```bash
git status
```

If files are already unlocked and the conversion does not complete as
expected, explicitly relock and unlock them:

```bash
git annex lock .
git annex unlock .
```

Ensure another verified copy exists before using `annex.thin`; it saves local
disk space but increases dependence on content available from other remotes.
