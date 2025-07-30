# shellcheck shell=bash

# Source global definitions
if [ -f /etc/bash.bashrc ]; then
    source /etc/bash.bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# Source additional files from .bashrc.d including user aliases and functions
for file in ~/.bashrc.d/*.sh; do
    if [ -r "$file" ] && [ -f "$file" ]; then
        # shellcheck source=/dev/null
        source "$file"
    fi
done

# Host-specific configuration
# Try to read HOSTNAME, fallback to /proc/sys/kernel/hostname if not set
: "${HOSTNAME:=$(cat /proc/sys/kernel/hostname)}"
# shellcheck source=/dev/null
[[ -r ~/.bashrc."$HOSTNAME" ]] && source "$HOME/.bashrc.$HOSTNAME"

# shellcheck source=/dev/null
[[ -r ~/.progs/bash_aliases ]] && source "$HOME/.progs/bash_aliases"

# shellcheck source=/dev/null
[[ -r ~/.hatch-complete.bash ]] && source "$HOME/.hatch-complete.bash"

# Set up color configurations for ls command
eval "$(dircolors -b)"
[[ $- == *i* ]] && bind -x '"\C-l": ls -lh'

export EDITOR='emacsclient -c -a=""'
export TERMINAL=kitty
export HISTCONTROL=ignoredups
export WINEARCH=win32
export R_LIBS_USER=~/.Renviron/
export IGNOREEOF=1
export MAKEFLAGS='-j4'

source /usr/share/bash-completion/completions/git
source /usr/share/bash-completion/completions/hg

shopt -s autocd       # Auto "cd" when entering just a path
shopt -s checkwinsize # Line wrap on window resize

# Pyenv initialization is common
eval "$(pyenv init -)"

# Prompt Configuration: Check for Starship or fallback to custom
if command -v starship &>/dev/null; then
    eval "$(starship init bash)"
else
    # shellcheck source=bash/.bashrc_custom_prompt.bash
    source "$HOME/.bashrc_custom_prompt.bash"
fi

# direnv is common
eval "$(direnv hook bash)"

# zoxide
eval "$(zoxide init bash)"

# GPG/SSH setup is common
GPG_TTY=$(tty)
export GPG_TTY
gpg-connect-agent updatestartuptty /bye >/dev/null

SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK
