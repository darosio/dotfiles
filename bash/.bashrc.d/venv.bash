#
# Manage virtual environments in ~/.local/venv/.
# Must be SOURCED for 'activate' to affect the current shell.
#
# Usage:
#   . venv.bash activate <name> [python_version]   # activate (creates with Python 3.13 if needed)
#   . venv.bash list                               # list available venvs
#

VENV_BASE="$HOME/.local/venv"

_venv_list() {
  if [ ! -d "$VENV_BASE" ] || [ -z "$(ls -A "$VENV_BASE" 2> /dev/null)" ]; then
    echo "No venvs found in $VENV_BASE"
    return 0
  fi
  echo "Available venvs in $VENV_BASE:"
  ls -1 "$VENV_BASE"
}

_venv_activate() {
  local name="$1"
  local pyver="${2:-3.13}"
  local path="$VENV_BASE/$name"

  if [ -d "$path" ]; then
    echo "Activating venv: $name"
  else
    echo "Venv '$name' not found. Creating at $path (Python $pyver)"
    mkdir -p "$VENV_BASE"
    uv venv --python "$pyver" "$path" || {
      echo "Error: Failed to create venv at $path" >&2
      return 1
    }
  fi

  # shellcheck disable=SC1091
  . "$path/bin/activate" || return 1

  _wrapped_uv_command() {
    local cmd=("$@")
    local post_install=false
    if [[ ${cmd[0]} == "pip" && ${cmd[1]} == "install" ]]; then
      post_install=true
    fi
    command uv "${cmd[@]}"
    local status=$?
    if $post_install && [ $status -eq 0 ]; then
      echo "Running 'uv pip freeze > requirements.txt' in $VIRTUAL_ENV..."
      (cd "$VIRTUAL_ENV" && command uv pip freeze > requirements.txt) ||
        echo "Warning: 'uv pip freeze' failed" >&2
    fi
    return $status
  }

  if command -v uv &> /dev/null; then
    alias uv='_wrapped_uv_command'
  else
    echo "Warning: 'uv' not found. Auto-freeze disabled." >&2
  fi
}

case "${1:-}" in
  activate)
    if [ -z "${2:-}" ]; then
      echo "Usage: . venv.bash activate <name> [python_version]" >&2
      return 1
    fi
    _venv_activate "$2" "${3:-}"
    ;;
  list)
    _venv_list
    ;;
  *)
    echo "Usage: . venv.bash <activate <name> [python_version]|list>" >&2
    return 1
    ;;
esac

return 0
