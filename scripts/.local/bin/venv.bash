#!/bin/bash
#
# This script activates a virtual environment from a centralized location
# (~/.local/venv/) or creates it there if it doesn't exist.
# It also defines a wrapper function for 'uv pip install' to automatically
# run 'uv pip freeze > requirements.txt' within the venv directory after installation.
#
# IMPORTANT: This script MUST BE SOURCED, not executed directly,
#            for the environment activation to affect your current shell.
#            Example: `. /path/to/this/script.sh <env_name> [additional_commands]`
#            or       `source /path/to/this/script.sh <env_name> [additional_commands]`
#
# Usage: . /path/to/script.sh <env_name> [additional_commands]
#

# Get the virtual environment name from the first argument
if [ -z "$1" ]; then
  echo "Usage: . /path/to/script.sh <env_name> [additional_commands]" >&2
  return 1 # Use 'return' instead of 'exit' when sourcing
fi
VENV_NAME="$1"
shift

# Define the centralized path
CENTRAL_VENV_PATH="$HOME/.local/venv/$VENV_NAME"

# Check if the centralized environment exists
if [ -d "$CENTRAL_VENV_PATH" ]; then
  echo "Activating centralized venv: $VENV_NAME"
  # Source the activate script directly into the current shell
  if [ -f "$CENTRAL_VENV_PATH/bin/activate" ]; then
    # shellcheck disable=SC1091
    . "$CENTRAL_VENV_PATH/bin/activate"
  else
    echo "Error: Activate script not found in $CENTRAL_VENV_PATH" >&2
    return 1
  fi
else
  # If the environment does not exist, create it in the centralized location
  echo "Centralized venv '$VENV_NAME' not found. Creating it at $CENTRAL_VENV_PATH"

  # Ensure the parent directory exists
  mkdir -p "$HOME/.local/venv"

  # Check if python and venv module are available for creation
  if command -v python > /dev/null 2>&1 && python -c "import venv" 2> /dev/null; then
    # Create the venv
    python -m venv "$CENTRAL_VENV_PATH" || {
      echo "Error: Failed to create venv at $CENTRAL_VENV_PATH" >&2
      return 1
    }
  else
    echo "Error: Cannot create a new venv - 'python' command or 'venv' module not available" >&2
    return 1
  fi

  # Activate the newly created environment
  if [ -f "$CENTRAL_VENV_PATH/bin/activate" ]; then
    # shellcheck disable=SC1091
    . "$CENTRAL_VENV_PATH/bin/activate"
  else
    echo "Error: Activate script not found after creation at $CENTRAL_VENV_PATH" >&2
    return 1
  fi
fi

# --- uv pip freeze automation ---
# Define a function to wrap the 'uv' command
# This function will check if the command is 'pip install' and then
# run 'uv pip freeze > requirements.txt' within the venv directory after installation.
_wrapped_uv_command() {
  # Store the original command and arguments
  local original_command=("$@")
  local post_install_action=false

  # Check if the command is 'pip install'
  # We check the first two arguments after 'uv'
  if [[ ${original_command[0]} == "pip" && ${original_command[1]} == "install" ]]; then
    post_install_action=true
  fi

  # Execute the original uv command using 'command' to avoid recursion
  # 'command uv' ensures we call the actual uv executable in the PATH, not this function
  command uv "${original_command[@]}"

  # Store the exit status of the uv command
  local uv_exit_status=$?

  # If the command was 'pip install' and it succeeded, run 'uv pip freeze'
  if $post_install_action && [ $uv_exit_status -eq 0 ]; then
    local venv_root="$VIRTUAL_ENV"
    local freeze_exit_status=0 # To store the exit status of uv pip freeze

    echo "Running 'uv pip freeze > requirements.txt' in $venv_root..."
    # Change directory to the venv root and run uv pip freeze
    (cd "$venv_root" && command uv pip freeze > requirements.txt)
    freeze_exit_status=$?
    if [ $freeze_exit_status -ne 0 ]; then
      echo "Warning: 'uv pip freeze' failed with exit status $freeze_exit_status" >&2
    fi
    # Return the original uv command's exit status
    return $uv_exit_status
  else
    # For other commands or if install failed, just return the uv command's exit status
    return $uv_exit_status
  fi
}

# Override the 'uv' command with our wrapper function
# This makes the function execute whenever you type 'uv'
# We only do this if the venv was successfully activated (VIRTUAL_ENV is set)
if [ -n "$VIRTUAL_ENV" ]; then
  # Check if uv is actually available in the activated environment's PATH
  if command -v uv &> /dev/null; then
    alias uv='_wrapped_uv_command'
    # Note: Using an alias is a simple way to override the command lookup.
    # A more robust method might involve manipulating the PATH or using
    # a trap, but alias is sufficient for most interactive uses.
  else
    echo "Warning: 'uv' command not found in the activated environment. Auto-freeze disabled." >&2
  fi
fi
# --- End uv pip freeze automation ---

# Execute additional commands if provided within the activated environment
# Note: 'exec' replaces the current shell, so if you want to return
#       to the interactive shell after commands, don't use 'exec'.
#       For sourcing, simply running the commands is usually sufficient.
if [ $# -gt 0 ]; then
  # Run the commands in the current shell after activation
  "$@"
  # If you want to prevent returning to the interactive prompt after commands, uncomment exec:
  # exec "$@"
fi

# Return 0 for success if no commands were executed or if commands succeeded
# If commands were executed, their exit status will be the script's exit status
return 0 # Return 0 on success when sourcing
