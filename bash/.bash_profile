# shellcheck shell=bash
PATH=$PATH:$HOME/.progs/git-annex.linux:$HOME/.ssh/
PATH=$PATH:~/workspace/HIV_pipeline_mpba/HIV/library_name/
PATH=$PATH:$HOME/.local/bin
export PATH

# shellcheck source=/dev/null
[[ -f ~/.profile ]] && source "$HOME"/.profile
# shellcheck source=/dev/null
[[ -f ~/.progs/bash_profile ]] && . "$HOME"/.progs/bash_profile
# shellcheck source=/dev/null
[[ -f ~/.bashrc ]] && . "$HOME"/.bashrc

systemctl --user import-environment PATH
# systemctl --user import-environment DISPLAY
#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
	startx
fi
