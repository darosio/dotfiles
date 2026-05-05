#!/bin/bash
# Bootstrap dotfiles installation
# Usage: ./install.sh [--all | --list | package1 package2...]

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SAFE_ALL_SCRIPTS=(
  0init.stow.sh
  0git_hg_package.stow.sh
  agents.stow.sh
  alacritty.stow.sh
  automount.stow.sh
  direnv.stow.sh
  emacs.stow.sh
  gh.stow.sh
  goldendict.stow.sh
  gvim.stow.sh
  htop.stow.sh
  mr.stow.sh
  pandoc.stow.sh
  pcmanfm.stow.sh
  pdf.stow.sh
  proselint_vale.stow.sh
  psd_firefox.stow.sh
  rclone.stow.sh
  recoll.stow.sh
  starship.stow.sh
  sway.stow.sh
  xdg-desktop-portal.stow.sh
  yazi.stow.sh
  zotero.stow.sh
)

MANUAL_SCRIPTS=(
  ai-containers.stow.sh
  fabric.stow.sh
  himalaya.stow.sh
  llm.stow.sh
  msmtp_sendmail.stow.sh
  mu.stow.sh
  wifite.stow.sh
)

# Ensure we're not using a virtual environment's python for system packages
deactivate_venv() {
  if [ -n "${VIRTUAL_ENV:-}" ]; then
    echo "Warning: Virtual environment detected ($VIRTUAL_ENV). Bypassing for system installation..."
    # Attempt to clean PATH by removing the venv bin directory
    PATH=${PATH//"$VIRTUAL_ENV/bin:"/}
    export PATH
    unset VIRTUAL_ENV
  fi
}

list_packages() {
  echo "Available packages:"
  echo ""
  echo "Bootstrap (run first):"
  find "$DOTFILES_DIR" -maxdepth 1 -name '0*.stow.sh' -printf '  %f\n' | sort
  echo ""
  echo "User packages (safe for --all):"
  printf '  %s\n' "${SAFE_ALL_SCRIPTS[@]}"
  echo ""
  echo "User packages (manual, side effects):"
  printf '  %s\n' "${MANUAL_SCRIPTS[@]}"
  echo ""
  echo "All user package scripts:"
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
  deactivate_venv
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
  echo "Installing safe user packages..."
  for script in "${SAFE_ALL_SCRIPTS[@]}"; do
    run_script "$script"
  done
  echo ""
  echo "Skipped manual packages with side effects:"
  printf '  %s\n' "${MANUAL_SCRIPTS[@]}"
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
    echo "  --all, -a    Install safe user packages"
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
