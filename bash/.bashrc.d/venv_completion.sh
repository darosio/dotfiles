#!/bin/bash
# Bash completion for the venv alias ('. ~/.bashrc.d/venv.bash').

_complete_central_venvs() {
  local cur prev venv_dir venv_names
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD - 1]}"
  venv_dir="$HOME/.local/venv"

  # First argument: subcommand
  if [ "$COMP_CWORD" -eq 1 ]; then
    # shellcheck disable=SC2207
    COMPREPLY=($(compgen -W "activate list" -- "$cur"))
    return 0
  fi

  # Second argument after 'activate': venv name
  if [ "$prev" = "activate" ] && [ -d "$venv_dir" ]; then
    mapfile -t venv_names < <(find "$venv_dir" -mindepth 1 -maxdepth 1 -type d -printf "%P\n")
    # shellcheck disable=SC2207
    COMPREPLY=($(compgen -W "${venv_names[*]}" -- "$cur"))
    return 0
  fi

  # Third argument after 'activate <name>': optional Python version
  if [ "$COMP_CWORD" -eq 3 ] && [ "${COMP_WORDS[1]}" = "activate" ]; then
    # shellcheck disable=SC2207
    COMPREPLY=($(compgen -W "3.13 3.12 3.11 3.10" -- "$cur"))
  fi

  return 0
}

# shellcheck disable=SC3044
complete -F _complete_central_venvs venv
