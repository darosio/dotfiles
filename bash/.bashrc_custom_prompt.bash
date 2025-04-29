#!/bin/bash
# Custom Bash Prompt Configuration
# This script is sourced by .bashrc when Starship is not found.

# store colors - needed for custom prompt
MAGENTA="\[\033[0;35m\]"
YELLOW="\[\033[01;33m\]"
BLUE="\[\033[00;34m\]"
LIGHT_GRAY="\[\033[0;37m\]"
CYAN="\[\033[0;36m\]"
GREEN="\[\033[00;32m\]"
RED="\[\033[0;31m\]"
RESET='\[\e[0m\]'

# Function to generate the custom PS1
function color_my_prompt {
    local __user_and_host="$RESET\h"
    local __cur_location="$BLUE\w"          # capital 'W': current directory, small 'w': full file path
    local __git_branch_color="$GREEN"
    local __prompt_tail="$RESET$"
    local __git_branch

    # Use __git_ps1 from the sourced git-prompt.sh
    # This function reads the exported GIT_PS1_* variables
    if command -v __git_ps1 &> /dev/null; then
       __git_branch=$(__git_ps1 "$@");
    else
       __git_branch=""
    fi

    # color branch name depending on state
    # shellcheck disable=SC2049,SC2076
    if [[ "${__git_branch}" =~ "*" ]]; then
        __git_branch_color="$RED"
    elif [[ "${__git_branch}" =~ "$" ]]; then
        __git_branch_color="$YELLOW"
    elif [[ "${__git_branch}" =~ "%" ]]; then
        __git_branch_color="$LIGHT_GRAY"
    elif [[ "${__git_branch}" =~ "+" ]]; then
        __git_branch_color="$CYAN"
    fi

    # Check for virtual environment
    if [[ -n "$VIRTUAL_ENV" ]]; then
        local __pyenv_prompt="$MAGENTA(${VIRTUAL_ENV##*/})$RESET"
    else
        local __pyenv_prompt=""
    # Check for Conda environment (alternative/additional check)
    # elif [[ -n "$CONDA_PREFIX" ]]; then
    #     local __conda_prompt="$BLUE($(basename "$CONDA_PREFIX"))$RESET"
    # else
    #     local __conda_prompt=""
    fi

    # Build the base PS1 (Prompt String)
    # Include Python/Conda prompt if set
    local base_ps1="$__user_and_host $__cur_location$__git_branch_color$__git_branch\n$__pyenv_prompt$__prompt_tail "
    # If you added Conda:
    # local base_ps1="$__user_and_host $__cur_location$__git_branch_color$__git_branch\n$__pyenv_prompt$__conda_prompt$__prompt_tail "


    # Add Ranger indicator if needed
    if [ -n "$RANGER_LEVEL" ]; then
        PS1="$base_ps1"'(in ranger) '
    else
        PS1="$base_ps1"
    fi
}

# If .git-prompt.sh exists, set options and source it
# These variables are read by the __git_ps1 function.
# Exporting them makes their intent clear to shellcheck and the sourced script.
if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWUPSTREAM="auto"
    export GIT_PS1_HIDE_IF_PWD_IGNORED=true
    export GIT_PS1_SHOWCOLORHINTS=true # Might conflict with custom colors, test this
    # shellcheck source=/dev/null
    source /usr/share/git/completion/git-prompt.sh
else
    echo "Warning: git-prompt.sh not found. Git information will not appear in the custom prompt." >&2
fi

# Configure PROMPT_COMMAND to use the custom prompt function
PROMPT_COMMAND=color_my_prompt
