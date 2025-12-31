#!/bin/bash
# Bootstrap dotfiles installation
# Usage: ./install.sh [--all | --list | package1 package2...]

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

list_packages() {
  echo "Available packages:"
  echo ""
  echo "Bootstrap (run first):"
  find "$DOTFILES_DIR" -maxdepth 1 -name '0*.stow.sh' -printf '  %f\n' | sort
  echo ""
  echo "User packages:"
  find "$DOTFILES_DIR" -maxdepth 1 -name '*.stow.sh' ! -name '0*' ! -name '2root*' -printf '  %f\n' | sort
  echo ""
  echo "Root/system packages (require sudo):"
  find "$DOTFILES_DIR" -maxdepth 1 -name '2root.*.sh' -printf '  %f\n' | sort
  echo ""
  echo "Standalone scripts:"
  find "$DOTFILES_DIR" -maxdepth 1 -name '*.sh' ! -name '*.stow.sh' ! -name '2root*' ! -name 'install.sh' -printf '  %f\n' | sort
}

run_script() {
  local script="$1"
  if [[ -x "$DOTFILES_DIR/$script" ]]; then
    echo "Running $script..."
    "$DOTFILES_DIR/$script"
  elif [[ -f "$DOTFILES_DIR/$script" ]]; then
    echo "Running $script..."
    bash "$DOTFILES_DIR/$script"
  else
    echo "Error: $script not found" >&2
    return 1
  fi
}

install_all() {
  echo "Installing all user packages..."
  for script in "$DOTFILES_DIR"/*.stow.sh; do
    [[ -f "$script" ]] || continue
    [[ "$(basename "$script")" == 2root* ]] && continue
    run_script "$(basename "$script")"
  done
  echo ""
  echo "Done! Root packages (2root.*.sh) must be run manually with sudo."
}

case "${1:-}" in
  --list | -l)
    list_packages
    ;;
  --all | -a)
    install_all
    ;;
  --help | -h | "")
    echo "Usage: $0 [--all | --list | package1.stow.sh package2.stow.sh...]"
    echo ""
    echo "Options:"
    echo "  --all, -a    Install all user packages"
    echo "  --list, -l   List available packages"
    echo "  --help, -h   Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 --list"
    echo "  $0 emacs.stow.sh gh.stow.sh"
    echo "  $0 --all"
    ;;
  *)
    for pkg in "$@"; do
      run_script "$pkg"
    done
    ;;
esac
