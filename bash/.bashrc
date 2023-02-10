# shellcheck shell=bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions
if [ -f /etc/bash.bashrc ]; then
	. /etc/bash.bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

##DD SSH_AUTH_SOCK=`ss -xl | grep -o '/run/user/1000/keyring-.*/ssh'`
##DD [ -z "$SSH_AUTH_SOCK" ] || export SSH_AUTH_SOCK
# To bind \C-s in ranger as forward search in readline is less useful
stty stop undef
alias ls='ls --color=auto'
# shellcheck source=/dev/null
source "$HOME/.functions"
# shellcheck source=/dev/null
source "$HOME/.aliases"
# shellcheck source=/dev/null
source "$HOME/.progs/bash_aliases"
# shellcheck source=/dev/null
[[ -r ~/.hatch-complete.bash ]] && source "$HOME/.hatch-complete.bash"

# shellcheck source=/dev/null
# host specific bash_profile
[[ -r .bashrc.$HOSTNAME ]] && . ".bashrc.$HOSTNAME"

[ -n "$RANGER_LEVEL" ] && PS1="$PS1"'(in ranger) '
cd "$AUTOCD" || return
[ -z "$PS1" ] && return

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
# turn off deprecated pyenv prompt
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
# store colors
MAGENTA="\[\033[0;35m\]"
YELLOW="\[\033[01;33m\]"
BLUE="\[\033[00;34m\]"
LIGHT_GRAY="\[\033[0;37m\]"
CYAN="\[\033[0;36m\]"
GREEN="\[\033[00;32m\]"
RED="\[\033[0;31m\]"
RESET='\[\e[0m\]'
function color_my_prompt {
  local __user_and_host="$RESET\h"
  local __cur_location="$BLUE\w"           # capital 'W': current directory, small 'w': full file path
  local __git_branch_color="$GREEN"
  local __prompt_tail="$RESET$"
  local __git_branch
  __git_branch=$(__git_ps1);
  # color branch name depending on state
  # shellcheck disable=2049,2076 # some magic PS1 which I don't understand well
  if [[ "${__git_branch}" =~ "*" ]]; then     # if repository is dirty
      __git_branch_color="$RED"
  elif [[ "${__git_branch}" =~ "$" ]]; then   # if there is something stashed
      __git_branch_color="$YELLOW"
  elif [[ "${__git_branch}" =~ "%" ]]; then   # if there are only untracked files
      __git_branch_color="$LIGHT_GRAY"
  elif [[ "${__git_branch}" =~ "+" ]]; then   # if there are staged files
      __git_branch_color="$CYAN"
  fi
  if [[ -n "$VIRTUAL_ENV" ]]; then
      export __pyenv_prompt="$MAGENTA(${VIRTUAL_ENV##*/})$RESET"   # Strip out the path and just leave the env name
  else
      export __pyenv_prompt=""
  fi
  # Build the PS1 (Prompt String)
  PS1="$__user_and_host $__cur_location$__git_branch_color$__git_branch\n$__pyenv_prompt$__prompt_tail "
}
# configure PROMPT_COMMAND which is executed each time before PS1
export PROMPT_COMMAND=(color_my_prompt)
# if .git-prompt.sh exists, set options and execute it
if [ -f /usr/share/git/completion/git-prompt.sh ]; then
  # shellcheck disable=SC2034
  GIT_PS1_SHOWDIRTYSTATE=true
  # shellcheck disable=SC2034
  GIT_PS1_SHOWSTASHSTATE=true
  # shellcheck disable=SC2034
  GIT_PS1_SHOWUNTRACKEDFILES=true
  # shellcheck disable=SC2034
  GIT_PS1_SHOWUPSTREAM="auto"
  # shellcheck disable=SC2034
  GIT_PS1_HIDE_IF_PWD_IGNORED=true
  # shellcheck disable=SC2034
  GIT_PS1_SHOWCOLORHINTS=true
  # shellcheck source=/dev/null
  source /usr/share/git/completion/git-prompt.sh
fi

#powerline-daemon -q
#POWERLINE_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. /usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh

# direnv
eval "$(direnv hook bash)"

GPG_TTY=$(tty)
export GPG_TTY
gpg-connect-agent updatestartuptty /bye > /dev/null
