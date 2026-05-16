#!/usr/bin/env bash

# broot shell integration:
# - provides the `br` helper which can change the current shell directory
# - leaves plain `broot` available for read-only / non-cd usage
if command -v broot > /dev/null 2>&1; then
  eval "$(broot --print-shell-function bash)"
fi
