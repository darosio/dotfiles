#!/bin/bash
# Bash completion for the centralized venv activation script (~/.local/bin/venv)
# This script uses Bash-specific features for programmable completion.

# Define the function that generates the list of possible completions
_complete_central_venvs() {
  local cur # The current word being completed
  local venv_dir="$HOME/.local/venv" # The directory containing the venvs
  local venv_names=() # Array to hold just the venv names

  # Initialize the completion array
  COMPREPLY=()

  # Get the current word being completed
  cur="${COMP_WORDS[COMP_CWORD]}"

  # Check if the venv directory exists
  if [ -d "$venv_dir" ]; then
    # Use find to get the list of all directory names within the venv directory.
    # -mindepth 1 -maxdepth 1: Only look at immediate subdirectories.
    # -type d: Only list directories.
    # -printf "%P\n": Print only the name relative to $venv_dir, followed by a newline.
    # Read the output into the venv_names array.
    # Using mapfile (or readarray) is generally preferred over command substitution for reading lines into an array.
    mapfile -t venv_names < <(find "$venv_dir" -mindepth 1 -maxdepth 1 -type d -printf "%P\n")

    # Use compgen -W (wordlist) to generate completions from the list of venv names,
    # filtering by the current word being typed ($cur).
    # The '--' is important to separate options from the word list.
    # shellcheck disable=SC2207 # This is a standard Bash completion idiom
    COMPREPLY=( $(compgen -W "${venv_names[*]}" -- "$cur") )

    # The previous approach using compgen -d directly listed full paths,
    # which is not the desired output format for COMPREPLY in this case.
  fi

  return 0
}

# Register the completion function for your alias or script name
# Replace 'venv' with the alias you use, or the script name if you source it directly
# If your alias is 'venv':
# shellcheck disable=SC3044 # 'complete' is a Bash built-in
complete -F _complete_central_venvs venv
