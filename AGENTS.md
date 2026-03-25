# AGENTS

Personal dotfiles managed with GNU Stow. See `README.md` for install,
`docs/` for detailed notes, `ai-containers/README.md` for containers.

______________________________________________________________________

## Principles

- **Prefer manual instructions over complex automation.** If a solution
  would be fragile or involve many moving parts, write a clear step-by-step
  guide instead. Automation that is hard to test or debug is worse than a
  well-documented manual procedure.
- Make precise, surgical changes. Do not refactor unrelated code.
- Run `make lint` (Python/shell) and `make type` after editing Python.
  Run `make test` to validate. All must pass before finishing.
- Dotfiles use GNU Stow. Each `*.stow.sh` script installs one package.
- Emacs config: declarations in `init.el`, config in `my-config/my-*.el`.
  AI/LLM config lives in `emacs/.emacs.d/my-config/my-ai.el`.

______________________________________________________________________

## Task routing (AI coding tools)

| Task                         | Tool                     | Model             |
| ---------------------------- | ------------------------ | ----------------- |
| Multi-file refactor / review | Claude Code              | `qwen3.5:35b-a3b` |
| Precise Python / proposal    | Claude Code              | `qwen3.5:27b`     |
| Hard reasoning (no tools)    | gptel                    | `deepseek-r1:32b` |
| Fast throwaway               | OpenCode / CLI           | `ministral-3:14b` |
| Large context (whole repo)   | Gemini CLI               | cloud             |
| Inline completion            | copilot.el (`prog-mode`) | —                 |
| Literature synthesis         | gptel `@search-science`  | `qwen3.5:35b-a3b` |
| Local web search (AI)        | `<Launch5> v` (Vane)     | `qwen3.5:35b-a3b` |

______________________________________________________________________

## Key paths

| Item                   | Path                                                   |
| ---------------------- | ------------------------------------------------------ |
| Emacs AI config        | `emacs/.emacs.d/my-config/my-ai.el`                    |
| MCP PDF server         | `emacs/.local/bin/pdf-mcp.py`                          |
| Grant prompt templates | `emacs/.emacs.d/prompts/grant-synthesis.org`           |
| Zotero BibTeX export   | `~/Sync/biblio/main.bib` (auto-exported by Zotero BBT) |
| Org-roam AI notes      | `~/Sync/notes/org-roam/ai-notes/`                      |
| AI container configs   | `ai-containers/`                                       |

______________________________________________________________________

## See also

- `docs/emacs.md` — Emacs AI stack, gptel presets, MCP tools, citation
  workflow, hardware baseline, model roster, daily workflow map
- `ai-containers/README.md` — Vane, SearxNG, Khoj container setup
- `README.md` — dotfiles install, stow packages
