# shellcheck shell=bash

export FZF_DEFAULT_OPTS="-e -m --sort 10000"
export FZF_DEFAULT_COMMAND="fd"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd -t d"
. /usr/share/fzf/key-bindings.bash
. /usr/share/fzf/completion.bash

# Functions using fzf

MANPATH=/usr/share/man

# Find and view man pages
fman() {
    f=$(fd . "$MANPATH/man${1:-1}" -t f -x echo '{/.}' | fzf) && man "$f"
}


# Install packages using pacman
fy() {
pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S
}

# Perform rga search and open selected files
frga() {
    RG_PREFIX="rga --files-with-matches"
    file="$(
        FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
            fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
                --phony -q "$1" \
                --bind "change:reload:$RG_PREFIX {q}" \
                --preview-window="70%:wrap"
    )"
    if [ -n "$file" ]; then
        echo "opening $file"
        rifle "$file"
    fi
}

# Kill processes
fkill() {
    pid="$(ps -ef | sed 1d | fzf -m --ansi --color fg:-1,bg:-1,hl:46,fg+:40,bg+:233,hl+:46 --color prompt:166,border:46  --border=sharp --prompt="➤  " --pointer="➤ " --marker="➤ "| awk '{print $2}')"
    echo "$pid"
	if [ "x$pid" != "x" ]
	then
        kill "-${1:-9}" "$pid"
	fi
}

# Search files using recoll
sf() {
    selected_file=$(recoll -t -b -a "$@" | fzf -m)
    if [ -n "$selected_file" ]; then
        xdg-open "$selected_file"
    fi
}
