# AI Containers

Local AI services stack managed with podman quadlets (systemd user
services) + stow. SearxNG, Vane and LiteLLM are quadlet units stowed from
`ai-containers/ai-containers/systemd/users/1000/`; khoj still runs on
podman-compose (quadlet evaluation units for it are transient, see Khoj).
`docker` commands work through the podman-docker shim (same daemonless
engine).

## Services

| Service         | URL                    | Purpose                                      |
| --------------- | ---------------------- | -------------------------------------------- |
| **Vane**        | http://127.0.0.1:3000  | AI-powered web search (Perplexica successor) |
| **SearxNG**     | http://127.0.0.1:8080  | Privacy-focused metasearch (MCP backend)     |
| **Khoj**        | http://127.0.0.1:42110 | Local RAG — index files, ask questions       |
| **LiteLLM**     | http://127.0.0.1:4000  | OpenAI-compatible model gateway              |
| **MCP-SearxNG** | stdio                  | Web search tool for gptel in Emacs           |

Use `127.0.0.1`, not `localhost`: the pasta port-forwarder behind quadlet
`PublishPort` resets IPv6 (`::1`) connections, and curl/Emacs try `::1`
first. `aic health` and the Emacs client config are set accordingly.

## Manage

```bash
aic up              # start all
aic up vane         # start one service
aic down khoj       # stop one
aic restart searxng # restart
aic enable          # start + autostart at boot (quadlet services)
aic url             # print service URLs
aic health          # check service reachability
aic update          # pull newer images + restart changed services
aic ps              # status
aic logs khoj       # follow logs
aic up litellm      # start the model gateway
```

The quadlet services are ordinary systemd user units, so plain systemctl
works too: `systemctl --user status searxng.service`,
`journalctl --user -u vane.service`.

## Keep-updates mechanism (the point of quadlets)

`podman-auto-update.timer` (enabled) fires `podman-auto-update.service`,
which runs `podman auto-update`: for containers labelled
`AutoUpdate=registry` it checks the registry digest, pulls, restarts, and
**rolls back to the old image if the restarted container fails** — then
prunes. searxng, vane and litellm carry the label.

The distro timer is daily; `ai-containers.stow.sh` installs a weekly
override (Mondays ~09:30) as `~/.config/systemd/user/podman-auto-update.timer`.

Not covered by design:

- khoj — migrations across 14-month drift need a human; update with
  `aic update khoj` after checking release notes.
- mcp-searxng / github — oneshot `podman run` servers started by Emacs;
  they pick up whatever image tag is current, so `podman image pull`
  (or `aic update`) is enough. No standing container to update.

Autostart: quadlet units have `WantedBy=default.target`; once enabled
(`aic enable`) they start at login/boot. User lingering is on
(`loginctl enable-linger dan`), so the user manager runs even without a
login session.

______________________________________________________________________

## Setup

### Deploy

```bash
./ai-containers.stow.sh   # stow configs+quadlet units, symlink
                          # ~/.config/containers/systemd -> ~/ai-containers/systemd,
                          # generate ~/.config/containers/{khoj,litellm}.env from pass
systemctl --user daemon-reload   # after unit-file changes
aic enable                # start quadlet services + autostart
```

`~/ai-containers` is a symlink into this repo worktree, so nothing secret
may live there; generated env files go to `~/.config/containers/`.

______________________________________________________________________

### Vane — http://127.0.0.1:3000

First visit opens a settings screen. Configure:

1. **Chat Model Provider** → Ollama
2. **Ollama API URL** → `http://host.containers.internal:11434`
   (rootless podman provides this name automatically; `host.docker.internal`
   is an alias of it)
3. **Chat Model** → `qwen3.6:35b-a3b`
4. **Embedding Model Provider** → Ollama
5. **Embedding Model** → `qwen3-embedding`

Vane bundles its own SearxNG — no additional search setup needed.
`my/vane--provider-id` in my-ai.el discovers the provider id from
`/api/config`, so the Ollama model choice there must match
`my/vane-chat-model` (currently `qwen3.6:35b-a3b`).

______________________________________________________________________

### SearxNG — http://127.0.0.1:8080

Pre-configured with scientific engines in `~/ai-containers/searxng/settings.yml`:

- PubMed, Google Scholar, Semantic Scholar, arXiv, CrossRef, Wolfram Alpha
- JSON output enabled (required by MCP-SearxNG)
- `brave`, `startpage`, `qwant` and `semantic scholar` are explicitly disabled,
  and `outgoing.request_timeout` is capped at 4s

The disables matter more than they look. SearXNG waits for every enabled engine
before returning, so engines that CAPTCHA or rate-limit you do not just
contribute nothing — they set the latency of *every* query. With those four
enabled a search took ~14s, which is longer than mcp-searxng waits, so
`searxng_web_search` aborted and gptel silently got no web results at all.
With them disabled the same query takes ~1s.

If web search starts failing again, check which engines are blocked before
touching anything else:

```bash
podman logs --since 2h searxng | grep -oE "Searx[A-Za-z]+Exception" | sort | uniq -c
curl -s "http://127.0.0.1:8080/search?q=test&format=json" | \
  python3 -c "import sys,json; d=json.load(sys.stdin); print(d['unresponsive_engines'])"
```

Disable whatever shows up there. To tweak engines:

```bash
$EDITOR ~/ai-containers/searxng/settings.yml
aic restart searxng
```

MCP-SearxNG (the stdio tool gptel uses) no longer runs as a standing
container — it died in place before (exit 137) and `podman exec` then fails
silently forever. `my-ai.el` now starts it oneshot per session:
`podman run -i --rm … isokoliuk/mcp-searxng`, pointed at
`SEARXNG_URL=http://host.containers.internal:8080`.

______________________________________________________________________

### Khoj — http://127.0.0.1:42110 (still podman-compose, under evaluation)

Khoj runs via podman-compose for now; its quadlet units exist as transient
evaluation copies under `$XDG_RUNTIME_DIR/containers/systemd/` (they vanish
on reboot; `aic up khoj` restores the service). Promote it once the
evaluation concludes: move the `.pod`+`.container` files into
`ai-containers/ai-containers/systemd/users/1000/` and restow.

Runs in anonymous mode (no login required for the main UI).
Admin panel at `http://127.0.0.1:42110/server/admin` — credentials in `pass ai/khoj`.

#### 1. AI Model API (pre-configured)

**Admin → AI Model APIs → Ollama** already points to `http://host.containers.internal:11434/v1/`.

#### 2. Chat Model — set default

**Admin → Server Chat Settings** → set **Chat default** to `qwen3.6:35b-a3b`.

All Ollama models are auto-discovered and listed under **Chat Models**.

#### 3. Embedding Model

**Admin → Search Model Configs → default → Edit:**

| Field                                 | Value                                       |
| ------------------------------------- | ------------------------------------------- |
| Bi encoder                            | `qwen3-embedding`                           |
| Embeddings inference endpoint         | `http://host.containers.internal:11434/v1/` |
| Embeddings inference endpoint type    | `openai`                                    |
| Embeddings inference endpoint API key | `ollama`                                    |

#### 4. Web Scraper

**Admin → Web Scrapers → Add:**

| Field    | Value                |
| -------- | -------------------- |
| Name     | `Jina`               |
| URL      | `https://r.jina.ai/` |
| Priority | `1`                  |

Jina Reader is free, no API key — prefixes URLs to return clean markdown.

#### 5. Speech-to-Text (optional)

**Admin → Speech to Text Model Options → Add:**

| Field        | Value                                |
| ------------ | ------------------------------------ |
| Model name   | `whisper-1`                          |
| Model type   | `openai`                             |
| AI Model API | Ollama (if `whisper` pulled locally) |

```bash
# Check if whisper is available
ollama pull whisper
```

Khoj degrades gracefully if no STT is configured.

#### 6. Index your files

Go to **http://127.0.0.1:42110** → Settings → Files:

- Add directories: `~/Sync/notes/`, `~/Sync/Grants/`, `~/manuscripts/`
- Khoj watches and re-indexes on changes

Or use the Emacs client (`M-s M-k`) to query directly.

______________________________________________________________________

### LiteLLM — http://127.0.0.1:4000

This optional gateway currently routes to Ollama on the host and exposes
stable role-based model names:

| Alias       | Ollama model             |
| ----------- | ------------------------ |
| `fast`      | `qwen3.6:35b-a3b`        |
| `writing`   | `qwen3.6:27b`            |
| `reasoning` | `qwen3.6:27b`            |
| `math`      | `phi4-reasoning:plus`    |
| `vision`    | `qwen3-vl:32b`           |
| `embedding` | `qwen3-embedding:latest` |

Start and test it (oneshot service, not enabled at boot):

```bash
aic up litellm
curl http://127.0.0.1:4000/health/liveliness
curl http://127.0.0.1:4000/v1/models
```

For an OpenAI-compatible client, use base URL
`http://127.0.0.1:4000/v1/`, API key `ollama`, and one of the aliases above.
For containerized clients, use `http://host.containers.internal:4000/v1/`.

The cloud routes read `OPENAI_API_KEY`, `OPENCODE_ZEN_API_KEY`,
`OPENCODE_GO_API_KEY` from `~/.config/containers/litellm.env`, regenerated
by `ai-containers.stow.sh` from pass (`home/openai-dpa`, and Zen/Go entries
when they exist — missing entries leave empty values and only their routes
fail). After editing pass entries: `./ai-containers.stow.sh && aic restart litellm`. Leave gptel's native backends unchanged unless a concrete routing
need arises.

#### Claude Code through LiteLLM

LiteLLM can translate Claude Code's Anthropic-compatible requests to OpenAI
or OpenCode Zen. These are API accounts, not ChatGPT/Claude web subscriptions.
OpenCode Zen is pay-as-you-go; the OpenCode Go monthly plan is a separate
service and may not expose the same models or endpoint.

Use one of the shell launchers (they only set env, no keys exported):

```bash
claude-litellm-openai
claude-litellm-zen
claude-litellm-go
claude-litellm-copilot
```

The launchers set `ANTHROPIC_BASE_URL=http://127.0.0.1:4000` and select the
`claude-openai`, `claude-zen`, or `claude-go` LiteLLM alias. The Zen route uses
`https://opencode.ai/zen/v1` and the Go route uses
`https://opencode.ai/zen/go/v1`. Change the model IDs in `litellm/config.yaml`
to models supported by your account.

The Copilot route uses LiteLLM's GitHub OAuth device flow and the
`github_copilot/gpt-4` model. On first use, watch the gateway logs:

```bash
aic restart litellm   # recreate after changing config.yaml
podman attach litellm  # keep this terminal attached during OAuth
```

Open the displayed GitHub URL, enter the device code, and authorize with the
GitHub account holding Copilot Pro. Keep the attach session open until the
server reports successful authentication. The OAuth token is stored in the
persistent `copilot_auth` volume. This route consumes Copilot quota and may
not expose all Claude Code features or models; if authentication still leaves
the directory empty, use the native `copilot.el`/Copilot CLI integration or
another Claude Code provider.

Do not route OpenCode itself through LiteLLM merely to use Zen. OpenCode has
native Zen authentication and model selection; use `/connect`, select Zen,
and then `/models`. LiteLLM is useful when Claude Code or another client must
share the same provider endpoint.

______________________________________________________________________

## Client Applications

### Emacs (primary)

`khoj.el` is configured in `emacs/.emacs.d/my-config/my-ai.el`:

```
M-s M-k   → open Khoj chat
```

Set `khoj-server-url` to `http://127.0.0.1:42110`.

### Obsidian

Install the **Khoj plugin** from Obsidian community plugins.
Set server URL to `http://127.0.0.1:42110` and API key to any non-empty string (anonymous mode).

### Browser

- **Vane** — add as browser search engine: `http://127.0.0.1:3000/?q=%s`
- **SearxNG** — add as browser search engine: `http://127.0.0.1:8080/?q=%s`
- **Khoj** — use directly at http://127.0.0.1:42110

### Mobile (Khoj app)

iOS/Android Khoj app available — set server to your machine's LAN IP:
`http://192.168.x.x:42110`

______________________________________________________________________

## Credentials

Khoj admin credentials are stored in `pass ai/khoj` and generated into
`~/.config/containers/khoj.env` by `ai-containers.stow.sh`. The env file is
never committed (`~/ai-containers` IS the repo worktree — do not put secrets
there).

To update credentials:

```bash
PASSWORD_STORE_DIR=~/Sync/.pass pass edit ai/khoj
./ai-containers.stow.sh
aic restart khoj
```
