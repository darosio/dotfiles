# AI tools cheat sheet

Quick reference for two recurring lookups when juggling multiple AI CLIs/editors:

1. **How do I switch model/provider *within* this tool** (e.g. use DeepSeek in
   Copilot CLI, or GitHub Models in OpenCode)?
2. **What can this tool do that I keep forgetting about** (skills, subagents,
   MCP servers, ...)?

For *which* tool/model to reach for on a given task, see the
[task routing table in `AGENTS.md`](../AGENTS.md#task-routing-ai-coding-tools)
and the [Daily Workflow table in `docs/emacs.md`](emacs.md#daily-workflow) —
this page is about switching providers *inside* a tool and rediscovering its
features, not about picking the tool itself.

## Switching model / provider within a tool

| Tool                   | In-session                                                    | Flag / one-off                           | Persistent config                                                                                                                                      |
| ---------------------- | ------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Copilot CLI**        | `/model`                                                      | n/a                                      | `/config model` (user default), `/model --repo`/`--local` (repo default), `/model plan`/`--plan` (plan-mode model); `/subagents` sets per-agent models |
| **Claude Code**        | `/model`                                                      | `claude --model <id>`                    | `ANTHROPIC_MODEL` env var, or alias `opus`/`sonnet`/`haiku`/`default`; via Ollama: `ollama launch claude --model qwen3.6:35b-a3b`                      |
| **OpenCode**           | `/models` (TUI)                                               | `opencode run --model provider/model`    | `"model": "provider/model"` in `opencode.json`; list options with `opencode models [provider]`                                                         |
| **Gemini CLI**         | `/model`                                                      | `gemini --model <id>` / `-m <id>`        | `"model": "..."` in `~/.gemini/settings.json`, or `GEMINI_MODEL` env var                                                                               |
| **Codex CLI**          | n/a                                                           | `codex --profile <name>`                 | `~/.codex/config.toml`: `[model_providers.<id>]` blocks + top-level `model_provider`/`model`, or `[profiles.<name>]`                                   |
| **Hermes**             | `/model` (switches only among *already configured* providers) | `hermes -m/--model <id> --provider <id>` | `hermes model` — interactive picker to *add* a new provider/key/OAuth (run this first if `/model` doesn't show it)                                     |
| **Emacs gptel/ellama** | gptel menu (`gptel-menu`, backend/model fields)               | —                                        | `my/ollama-model` helper + `my/ollama-*-model` constants in `my-ai.el` (see `docs/emacs.md`)                                                           |

Key gotcha (Hermes, but a useful mental model generally): the in-session
switcher only lists providers you've *already* authenticated. If a provider
is missing from `/model`, run the tool's dedicated "add provider" command
first (`hermes model`; analogous to `claude mcp add`/`opencode auth login`
for their respective ecosystems).

## Notable skills / features per tool (the "wait, which tool had that?" list)

| Tool        | Feature                                                       | What it's for                                                                                                                |
| ----------- | ------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| Copilot CLI | `/skills`                                                     | Manage/enable skills for enhanced capabilities (this repo's `find-skills`, `customize-cloud-agent`, `github-pr-media`, etc.) |
| Copilot CLI | `/agent`, `/fleet`, `/delegate`                               | Browse/select subagents; parallel subagent execution; hand a session off to GitHub to open a PR                              |
| Copilot CLI | `/plan`, `/research`                                          | Build an implementation plan before coding; deep research via GitHub + web search                                            |
| Copilot CLI | `/rubber-duck`, `/review`, `/security-review`, `/diagnose`    | Independent critique of current work; code/security review of a diff; analyze session logs                                   |
| Copilot CLI | `/mcp`, `/lsp`                                                | Manage MCP servers / language servers                                                                                        |
| Claude Code | **Skills** (e.g. `dataviz`)                                   | Packaged capability extensions — check `~/.claude` / skill marketplace before assuming a feature is Copilot/Hermes-only      |
| Claude Code | Custom slash commands (`agents/.claude/commands/`)            | `/docstring`, `/typecheck`, `/fitreview` — project-specific workflows defined in this repo                                   |
| Claude Code | MCP servers (`agents/.claude/setup-mcp-servers.sh`)           | `zotero` (local Zotero library), `pdf` (PyMuPDF-based PDF tool)                                                              |
| OpenCode    | 75+ providers, best TUI                                       | Go-to when you want provider flexibility without leaving one tool                                                            |
| Gemini CLI  | 1M token context, 1000 free req/day                           | Whole-repo / large-context tasks without API cost                                                                            |
| Codex CLI   | Named profiles (`~/.codex/config.toml`)                       | Fast switching between e.g. a cheap/quick profile and a deep-reasoning profile                                               |
| Hermes      | Skills (`-s`/`--skills`), toolsets (`-t`/`--toolsets`)        | Preload capabilities/tools per run instead of globally enabling everything                                                   |
| Hermes      | `hermes gateway` (Telegram/Discord/Slack), `hermes dashboard` | Same agent/memory reachable outside the terminal — useful when away from a CLI                                               |
| Hermes      | Persistent cross-surface memory                               | Context carries over between terminal, Telegram, and dashboard sessions                                                      |

## Secrets from `pass` (single registry for all agents)

API keys live only in the password store (`~/Sync/.pass`). One registry
file decides which entry feeds which tool; adding Anthropic/OpenAI/
OpenRouter/... is ONE line there, never a script edit:

```
~/.config/agent-secrets/registry   (stowed from agents/.config/)
# ENV_VAR  pass-entry  [pi-provider-id]
OPENAI_API_KEY   cloud/openai   openai     # <- uncomment when the entry exists
```

`~/.local/bin/agent-secrets` (stowed) is the resolver — `list` (registry
rows), `emit` (VAR=VALUE blob), `agent-secrets cloud/x` (one entry).
Consumers:

| Tool     | How it gets keys                                                                                                                                                   | Stored secret                                                                               |
| -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------- |
| Hermes   | `secrets.command` → `hermes-secrets` → `agent-secrets emit` (all rows)                                                                                             | none                                                                                        |
| pi       | `pi-pass-auth` writes every row with a pi-provider-id as a command key: `"!\"$HOME/.local/bin/agent-secrets\" cloud/x"`                                            | command reference, no literal key                                                           |
| OpenCode | paste the pass value ONCE into its own store: `/connect` → provider → key, or the PTY helper below. Its db then serves every surface (TUI, `run`, shared service). | key in opencode.db (mode 600), same class as its existing deepseek/openrouter/copilot creds |

OpenCode v2 has no `!command` reference form (pi's trick) and its shared
background service never inherits a wrapper's env, so the wrapper
`opencode-with-secrets` (`--standalone` + exported env) is only a fallback for
headless `run` — it does not populate the shared service's model list. The
durable setup is the stored credential. Re-add after a rotation (the key
never passes through chat — the helper reads `pass` directly):

```sh
python3 - <<'PY'   # PTY helper: types the pass value into `opencode auth login`
import os, pty, time, select, fcntl, termios, struct, re
key = os.popen(os.path.expanduser("~/.local/bin/agent-secrets cloud/qwencloud")).read().strip()
pid, fd = pty.fork()
if pid == 0:
    os.execvp("opencode", ["opencode", "auth", "login", "alibaba-token-plan", "--method", "key"])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))
time.sleep(3.0); os.write(fd, (key + "\r").encode()); key = ""
deadline = time.time() + 30
while time.time() < deadline:
    r, _, _ = select.select([fd], [], [], 1)
    if not r: break
    try: d = os.read(fd, 4096)
    except OSError: break
    if not d: break
os.close(fd); os.waitpid(pid, 0)
PY
opencode auth list --format json | jq -r '.[] | select(.id|test("alibaba")) | .id'
```

The `sleep 3` then write is the reliable part: the TUI renders one character
per write with ANSI escapes interleaved, so matching on a prompt string races.
Set a real window size first (`TIOCSWINSZ`) or the TUI never paints.

Workflow for a new provider key:

1. `pass insert cloud/openai` (first line = the key)
2. Add `OPENAI_API_KEY cloud/openai openai` to the registry
3. `stow -t ~ agents && pi-pass-auth` — done; restart the agents
   (Hermes re-runs the helper per start; pi caches keys per process).

Gotchas:

- Locked gpg-agent = pinentry = helper timeout: every tool sees "not
  configured", never corruption. Quick check: `pass show cloud/qwencloud >/dev/null`.
- The pi mapping needs pi's provider ID (docs/providers.md env-var table —
  e.g. anthropic→`anthropic`, openai→`openai`, google→`google`). Rows
  without a third column are Hermes-only.
- pi: leave the third column empty for Anthropic while Claude Code OAuth is
  your plan; a command key there would outrank the OAuth credentials.
- OpenCode: the shared background service never sees the exported env — use
  the wrapper (`--standalone` goes last, which the wrapper appends for you).
  Rows OpenCode doesn't know (EXA, TELEGRAM) are filtered via
  `~/.cache/opencode/models.json` `env` arrays.
- pi `--print` against the token-plan endpoint has a pre-existing bug:
  HTTP 400 "developer is not one of [...]" with `--model auto` (system role
  mapped to `developer`). Workaround: pin `--provider qwen-token-plan-individual --model <id>`; interactive mode is unaffected.
- `opencode auth list` may still show OAuth logins (copilot) — those stay in
  the db by design; command-key auth only replaces pasted API keys.

## Notes

- This list reflects the tools set up in this repo (`agents/` stow package:
  `.claude`, `.copilot`, `.codex`, `.gemini`, `.hermes`) plus Emacs
  gptel/ellama/Khoj integration documented in `docs/emacs.md`.
- Keep this updated when a tool gains/loses a feature you rely on, or when
  you discover a skill/command you didn't know existed — that's the whole
  point of this doc.
