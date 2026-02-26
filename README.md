# Dotfiles

[![CI](https://github.com/darosio/dotfiles/actions/workflows/ci.yml/badge.svg)](https://github.com/darosio/dotfiles/actions/workflows/ci.yml)
[![Emacs CI](https://github.com/darosio/dotfiles/actions/workflows/update-emacs.yml/badge.svg)](https://github.com/darosio/dotfiles/actions/workflows/update-emacs.yml)

Configuration files for a Linux desktop managed with
[GNU Stow](https://www.gnu.org/software/stow/) and versioned with Git. Includes
Emacs (straight.el), shell, git, and application configs plus Python utility
scripts under `src/`.

## Quick Start

**Prerequisites:** [uv](https://docs.astral.sh/uv/),
[direnv](https://direnv.net/), [GNU Stow](https://www.gnu.org/software/stow/)

```bash
git clone https://github.com/darosio/dotfiles.git ~/workspace/dotfiles
cd ~/workspace/dotfiles
direnv allow                          # creates venv, installs deps
make init                             # installs pre-commit hooks
```

### Installing Packages

```bash
./install.sh --list                   # see available packages
./install.sh --all                    # install all user packages
./install.sh emacs.stow.sh gh.stow.sh  # install specific packages
```

Or use `stow` directly:

```bash
stow emacs                            # symlinks emacs/ into $HOME
stow -t /home mr                      # custom target directory
```

### Script Naming Convention

| Prefix       | Description                        | Example         |
| ------------ | ---------------------------------- | --------------- |
| `0*.stow.sh` | Bootstrap/init scripts (run first) | `0init.stow.sh` |
| `*.stow.sh`  | User stow scripts for `$HOME`      | `emacs.stow.sh` |
| `2root.*.sh` | Root/system configs (require sudo) | `2root.sshd.sh` |
| `*.sh`       | Standalone setup scripts           | `audio.sh`      |

## Development

The project uses [uv](https://docs.astral.sh/uv/) for Python dependency
management and [direnv](https://direnv.net/) for automatic environment
activation.

### Make Targets

Run `make help` for the full list:

```
  init            Install pre-commit hooks
  lint            Lint codebase with pre-commit
  type            Type-check Python scripts with mypy
  test            Run tests with pytest and coverage
  check           Run all checks (lint + type + test + cov)
  bump            Bump version, update changelog, and tag release
  test-emacs      Run Emacs smoke tests
  upgrade-emacs   Full upgrade: update + smoke tests
  thaw-emacs      Restore straight.el repos to match lockfile
  clean           Remove temporary and cache files
```

### Commit Conventions

Follow [Conventional Commits](https://www.conventionalcommits.org/). Scopes
reflect the configuration domain:

```
feat(emacs): add consult-ripgrep binding
fix(git): correct delta pager config
chore(deps): update uv.lock
```

### Testing

```bash
make test                              # Python script tests
make test-emacs                        # Emacs smoke tests (~70 checks)
make check                             # lint + type + test + coverage
```

## Emacs Package Management

Emacs packages are managed with
[straight.el](https://github.com/radian-software/straight.el) and pinned via
`emacs/.emacs.d/straight/versions/default.el`.

### Upgrading Packages

**Locally:**

```bash
make upgrade-emacs    # pull → normalize → freeze → thaw → check → smoke tests
# review changes to emacs/.emacs.d/straight/versions/default.el
git add -A && git commit -m "bump(emacs): update straight.el packages"
git push
```

**Via CI:** trigger the *Emacs CI* workflow manually (`workflow_dispatch`). It
runs `make upgrade-emacs` and creates a PR if versions changed.

**After merging a CI-created PR:**

```bash
git pull
make thaw-emacs       # checkout pinned commits + rebuild
```

## CI / Automation

| Workflow              | Trigger          | Purpose                                 |
| --------------------- | ---------------- | --------------------------------------- |
| `ci.yml`              | push, PR         | pre-commit lint, pytest, auto-merge     |
| `update-emacs.yml`    | dispatch, push   | Emacs smoke tests / package update + PR |
| `cruft-update.yml`    | weekly, dispatch | Apply cookiecutter template updates     |
| `lockfile-update.yml` | weekly, dispatch | `uv lock --upgrade` + PR                |
| `release.yml`         | tag push         | Publish release                         |

## License

[BSD-3-Clause](LICENSE.txt)

## Documentation

Full documentation (Emacs packages, submodule management, scripts reference) is
available in the `docs/` directory. Build locally with:

```bash
make docs
make docs-serve       # http://localhost:8000
```
