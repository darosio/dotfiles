#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

##DD SSH_AUTH_SOCK=`ss -xl | grep -o '/run/user/1000/keyring-.*/ssh'`
##DD [ -z "$SSH_AUTH_SOCK" ] || export SSH_AUTH_SOCK

alias ls='ls --color=auto'

source $HOME/.aliases
source $HOME/.progs/bash_aliases
# host specific bash_profile
[[ -r .bashrc.$HOSTNAME ]] && . .bashrc.$HOSTNAME

#PS1='[\u@\h \W]\$ '
source /usr/share/git/completion/git-prompt.sh
PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM=1
[ -n "$RANGER_LEVEL" ] && PS1="$PS1"'(in ranger) '
cd "$AUTOCD"


#powerline-daemon -q
#POWERLINE_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. /usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh


#eval "$(keychain --eval --agents ssh,gpg C08BC79E3A3F1D89AD90C0008B6A39EFA290FB41)"
export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye > /dev/null

#eval "$(fasd --init auto)"

