#!/usr/bin/env sh
#

if [ -n "$VIRTUAL_ENV" ]; then
  PATH="$(echo "$PATH" | sed -e "s|$VIRTUAL_ENV/bin:||g")"
  export PATH
  unset VIRTUAL_ENV
fi

uv tool install llm --with llm-ollama --with llm-templates-fabric --with llm-tools-rag
yay -S --noconfirm python-pymupdf
