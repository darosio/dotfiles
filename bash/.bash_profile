#
# ~/.bash_profile
#

PATH=$PATH:$HOME/.progs/git-annex.linux:$HOME/.ssh/
PATH=$PATH:~/workspace/HIV_pipeline_mpba/HIV/library_name/
PATH=$PATH:$HOME/.local/bin
source $HOME/.progs/bash_profile
export PATH

[[ -f ~/.bashrc ]] && . ~/.bashrc

#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then 
	startx
fi
