# AI Containers

Local AI services stack managed with podman-compose and stow.

## Services

| Service         | URL                    | Purpose                                      |
| --------------- | ---------------------- | -------------------------------------------- |
| **Vane**        | http://localhost:3000  | AI-powered web search (Perplexica successor) |
| **SearxNG**     | http://localhost:8080  | Privacy-focused metasearch (MCP backend)     |
| **Khoj**        | http://localhost:42110 | Local RAG — index files, ask questions       |
| **LiteLLM**     | http://localhost:4000  | OpenAI-compatible model gateway              |
| **MCP-SearxNG** | stdio                  | Web search tool for gptel in Emacs           |

## Manage

```bash
aic up              # start all
aic up vane         # start one service
aic down khoj       # stop one
aic restart searxng # restart
aic url             # print service URLs
aic health          # check service reachability
aic update          # pull latest images + recreate services
aic ps              # status
aic logs khoj       # follow logs
aic up litellm       # start the model gateway
```

______________________________________________________________________

## Setup

### Deploy

```bash
./ai-containers.stow.sh   # stow configs + generate khoj .env from pass
aic up                    # start all services
```

______________________________________________________________________

### Vane — http://localhost:3000

First visit opens a settings screen. Configure:

1. **Chat Model Provider** → Ollama
2. **Ollama API URL** → `http://host.docker.internal:11434`
3. **Chat Model** → `qwen3.6:35b-a3b`
4. **Embedding Model Provider** → Ollama
5. **Embedding Model** → `qwen3-embedding`

Vane bundles its own SearxNG — no additional search setup needed.

______________________________________________________________________

### SearxNG — http://localhost:8080

Pre-configured with scientific engines in `~/ai-containers/searxng/settings.yml`:

- PubMed, Google Scholar, Semantic Scholar, arXiv, CrossRef, Wolfram Alpha
- JSON output enabled (required by MCP-SearxNG)

To tweak engines:

```bash
$EDITOR ~/ai-containers/searxng/settings.yml
aic restart searxng
```

______________________________________________________________________

### Khoj — http://localhost:42110

Runs in anonymous mode (no login required for the main UI).
Admin panel at `http://localhost:42110/server/admin` — credentials in `pass ai/khoj`.

#### 1. AI Model API (pre-configured)

**Admin → AI Model APIs → Ollama** already points to `http://host.docker.internal:11434/v1/`.

#### 2. Chat Model — set default

**Admin → Server Chat Settings** → set **Chat default** to `qwen3.6:35b-a3b`.

All Ollama models are auto-discovered and listed under **Chat Models**.

#### 3. Embedding Model

**Admin → Search Model Configs → default → Edit:**

| Field                                 | Value                                   |
| ------------------------------------- | --------------------------------------- |
| Bi encoder                            | `qwen3-embedding`                       |
| Embeddings inference endpoint         | `http://host.docker.internal:11434/v1/` |
| Embeddings inference endpoint type    | `openai`                                |
| Embeddings inference endpoint API key | `ollama`                                |

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

______________________________________________________________________

### LiteLLM — http://localhost:4000

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

Start and test it:

```bash
aic up litellm
curl http://localhost:4000/health/liveliness
curl http://localhost:4000/v1/models
```

For an OpenAI-compatible client, use base URL
`http://localhost:4000/v1/`, API key `ollama`, and one of the aliases above.
For containerized clients, use `http://host.containers.internal:4000/v1/`.

The initial configuration intentionally contains no cloud providers or API
keys. After validating the local route, migrate Khoj, paper-qa, fabric, and
Vane one at a time by changing only their endpoint and model name. Leave
gptel's native backends unchanged unless a concrete routing need arises.

#### Claude Code through LiteLLM

LiteLLM can translate Claude Code's Anthropic-compatible requests to OpenAI
or OpenCode Zen. These are API accounts, not ChatGPT/Claude web subscriptions.
OpenCode Zen is pay-as-you-go; the OpenCode Go monthly plan is a separate
service and may not expose the same models or endpoint.

Store credentials outside Git, export them before starting LiteLLM, and then
start the gateway:

```bash
export OPENAI_API_KEY="$(pass show home/openai-dpa | head -1)"
export OPENCODE_ZEN_API_KEY="$(pass show cloud/opencode_zen | head -1)"
export OPENCODE_GO_API_KEY="$(pass show cloud/opencode_go | head -1)"
aic up litellm
```

Use one of the shell launchers:

```bash
claude-litellm-openai
claude-litellm-zen
claude-litellm-go
claude-litellm-copilot
```

The launchers set `ANTHROPIC_BASE_URL=http://localhost:4000` and select the
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

#### 6. Index your files

Go to **http://localhost:42110** → Settings → Files:

- Add directories: `~/Sync/notes/`, `~/Sync/Grants/`, `~/manuscripts/`
- Khoj watches and re-indexes on changes

Or use the Emacs client (`M-s M-k`) to query directly.

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
Set server URL to `http://localhost:42110` and API key to any non-empty string (anonymous mode).

### Browser

- **Vane** — add as browser search engine: `http://localhost:3000/?q=%s`
- **SearxNG** — add as browser search engine: `http://localhost:8080/?q=%s`
- **Khoj** — use directly at http://localhost:42110

### Mobile (Khoj app)

iOS/Android Khoj app available — set server to your machine's LAN IP:
`http://192.168.x.x:42110`

______________________________________________________________________

## Credentials

Khoj admin credentials are stored in `pass ai/khoj` and generated into
`~/ai-containers/khoj/.env` by `ai-containers.stow.sh`. The `.env` file
is gitignored and never committed.

To update credentials:

```bash
PASSWORD_STORE_DIR=~/Sync/.pass pass edit ai/khoj
aic restart khoj
```
