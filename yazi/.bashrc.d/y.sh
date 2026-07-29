#!/usr/bin/env bash
function y() {
  local tmp cwd
  tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
  yazi "$@" --cwd-file="$tmp"
  IFS= read -r -d '' cwd < "$tmp"
  # Never `return` before the cleanup below: an early return whenever the cwd is
  # unchanged (the common case) leaks a /tmp/yazi-cwd.* file per run.
  if [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    builtin cd -- "$cwd" || printf 'y: cannot cd to %s\n' "$cwd" >&2
  fi
  rm -f -- "$tmp"
}
