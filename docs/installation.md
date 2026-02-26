# Installation

## Prerequisites

- [uv](https://docs.astral.sh/uv/) — Python package manager
- [direnv](https://direnv.net/) — automatic environment activation
- [GNU Stow](https://www.gnu.org/software/stow/) — symlink farm manager

## Clone and Setup

```bash
git clone https://github.com/darosio/dotfiles.git ~/workspace/dotfiles
cd ~/workspace/dotfiles
direnv allow          # creates venv, installs deps
make init             # installs pre-commit hooks
```

## Installing Packages

```bash
./install.sh --list                     # see available packages
./install.sh --all                      # install all user packages
./install.sh emacs.stow.sh gh.stow.sh  # install specific packages
```

Or use `stow` directly:

```bash
stow emacs              # symlinks emacs/ into $HOME
stow -t /home mr        # custom target directory
```

### Script Naming Convention

| Prefix       | Description                        | Example         |
| ------------ | ---------------------------------- | --------------- |
| `0*.stow.sh` | Bootstrap/init scripts (run first) | `0init.stow.sh` |
| `*.stow.sh`  | User stow scripts for `$HOME`      | `emacs.stow.sh` |
| `2root.*.sh` | Root/system configs (require sudo) | `2root.sshd.sh` |
| `*.sh`       | Standalone setup scripts           | `audio.sh`      |
