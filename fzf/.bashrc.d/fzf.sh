# shellcheck shell=bash

eval "$(fzf --bash)"

# Exclusions live in ~/.local/bin/fzf-source and reach every picker through the
# commands below. --walker-skip only matters when that script cannot run and fzf
# falls back to its built-in walker; it is a rough echo of the script, limited to
# bare directory names since the walker cannot match paths. It is absolute rather
# than additive, so fzf's own default (.git,node_modules) has to be repeated or
# lost. ctrl-o re-lists with nothing excluded, the escape hatch on either route.
export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS:+$FZF_DEFAULT_OPTS }--walker-skip=.git,node_modules,.cache,site-packages,.venv,venv,.hatch,__pycache__,.goldendict --bind='ctrl-o:reload(fd --hidden --no-ignore --strip-cwd-prefix)'"

# Guarded so an unstowed machine still gets a working fzf rather than a picker
# whose source command fails.
if command -v fd > /dev/null 2>&1 && [ -x "$HOME/.local/bin/fzf-source" ]; then
  export FZF_DEFAULT_COMMAND="$HOME/.local/bin/fzf-source"
  # Ctrl-T and Alt-C blank FZF_DEFAULT_COMMAND unless given one of their own,
  # which would drop them back to the walker and its separate list. Pointing
  # them here keeps fzf-source the single source of truth.
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND --type d"
fi

# Find and view man pages
MANPATH=/usr/share/man
fman() {
  f=$(fd . "$MANPATH/man${1:-1}" -t f -x echo '{/.}' | fzf) && man "$f"
}

# Install packages using pacman
fy() {
  pacman -Sql | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S
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
  pid="$(ps -ef | sed 1d | fzf -m --ansi --color fg:-1,bg:-1,hl:46,fg+:40,bg+:233,hl+:46 --color prompt:166,border:46 --border=sharp --prompt="➤  " --pointer="➤ " --marker="➤ " | awk '{print $2}')"
  echo "$pid"
  if [ -n "$pid" ]; then
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

fzf_open() {
  local file
  file=$(fzf -e)
  [[ -n $file ]] && xdg-open "$file"
}

fzf_cmd() {
  compgen -c | sort -u | fzf
}
