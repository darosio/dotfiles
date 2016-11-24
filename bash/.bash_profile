#
# ~/.bash_profile
#

export PATH=$PATH:$HOME/.progs/git-annex.linux:$HOME/.ssh/

[[ -f ~/.bashrc ]] && . ~/.bashrc

#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then 
	startx
fi

#export DESKTOP_SESSION=LXDE
