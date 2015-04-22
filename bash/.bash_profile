#
# ~/.bash_profile
#

export PATH=$PATH:$HOME/.progs/git-annex.linux:$HOME/.ssh/

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

#export DESKTOP_SESSION=LXDE
