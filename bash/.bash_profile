#
# ~/.bash_profile
#

PATH=$PATH:$HOME/.progs/git-annex.linux:$HOME/.ssh/
PATH=$PATH:~/workspace/HIV_pipeline_mpba/HIV/library_name/
PATH=$PATH:$HOME/.local/bin
export PATH


[[ -f ~/.profile ]] && source /home/dan/.profile
[[ -f ~/.progs/bash_profile ]] && . /home/dan/.progs/bash_profile
[[ -f ~/.bashrc ]] && . "$HOME/.bashrc"

systemctl --user import-environment PATH
# systemctl --user import-environment DISPLAY

#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then 
	startx
fi
