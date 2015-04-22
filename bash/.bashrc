#
# ~/.bashrc
#

PATH=$PATH:$HOME/.progs/git-annex.linux
export PATH

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

##DD SSH_AUTH_SOCK=`ss -xl | grep -o '/run/user/1000/keyring-.*/ssh'`
##DD [ -z "$SSH_AUTH_SOCK" ] || export SSH_AUTH_SOCK

alias ls='ls --color=auto'
source $HOME/.aliases

#PS1='[\u@\h \W]\$ '
source /usr/share/git/completion/git-prompt.sh
PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM=1

# host specific bash_profile                                                    
[[ -r .bashrc.$HOSTNAME ]] && . .bashrc.$HOSTNAME
