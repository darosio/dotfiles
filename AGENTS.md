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

| Item                   | Path                                                                            |
| ---------------------- | ------------------------------------------------------------------------------- |
| Emacs AI config        | `emacs/.emacs.d/my-config/my-ai.el`                                             |
| MCP PDF server         | `emacs/.local/bin/pdf-mcp.py`                                                   |
| Grant prompt templates | `emacs/.emacs.d/prompts/grant-synthesis.org`                                    |
| Zotero BibTeX export   | `~/Sync/biblio/main.bib`, `MY.bib`, `former.bib` (BBT auto-export)              |
| Biblio PDFs            | `~/Sync/biblio/main/KEY/file.pdf` (Zotmoov subdir layout)                       |
| Citar notes            | `~/Sync/notes/org-roam/biblio/KEY.org` → migrating to `biblio/main/KEY/KEY.org` |
| Org-roam AI notes      | `~/Sync/notes/org-roam/ai-notes/`                                               |
| Zotero stow config     | `zotero/.zotero/zotero/90bt0tu8.default/user.js`, `zotero/treePrefs.json`       |
| AI container configs   | `ai-containers/`                                                                |

______________________________________________________________________

## Zotero / citation workflow state

### BBT citekey format

Formula: `auth.capitalize + year2 + shorttitle(3)` → e.g. `Alejaldre21MethodsEnzymeLibrary`
Set in `user.js`. After changing, do: **Zotero → Edit → Better BibTeX → Refresh all keys**,
then `M-x org-roam-db-sync` in Emacs.

### Auto-exports (user.js)

- `main.bib` — collection id 9
- `MY.bib` — collection id 12
- `former.bib` — collection id 8

### treePrefs.json (column layout)

Stored at `zotero/treePrefs.json` — **copied** (not symlinked) by `zotero.stow.sh`
because Zotero overwrites it atomically at runtime.
Columns: title, firstCreator, year, dateAdded, citationcounts, readstatus, hasAttachment.

### parsebib skip-invalid advice

`main.bib` has BBT-unresolved entries (no cite key). The advice in
`with-eval-after-load 'parsebib` catches `parsebib-error` and skips bad entries.

### citar PDF opener

`citar-file-open-functions`: `pdf` and `html` → `citar-file-open-external` (zathura);
`t` → `find-file`. The old sqlite3 lambda (singular `citar-file-open-function`) was removed.

### citar-llm-summarize

`my/citar-llm-summarize` in `my-ai.el`, bound to `C-c b s`.
`citar-get-files` returns a hash-table — use `(gethash key ht)` to extract the file list.

### Zotero plugins (assessed)

- **Better BibTeX** ✅ essential

- **Actions & Tags** ✅ keep (auto-tag unread, Ctrl+l → zotero:// URI)

- **Better Notes** ⚠️ marginal — duplicates org-roam workflow

- **PapersGPT** ❌ remove — superseded by `my/citar-llm-summarize` + gptel

- [ ] copy prompt from https://github.com/papersgpt/papersgpt-for-zotero/blob/bb5ba7057e1245622aee80dbf189127f61eeb0b9/src/modules/base.ts#L194

- postscript for better bibtex export
  if (Translator.BetterBibTeX && item.attachments) {
  let zoteroLinks = [];
  for (const att of item.attachments) {
  // Check if it's a PDF and has a key
  if ((att.contentType === 'application/pdf' || att.filename.toLowerCase().endsWith('.pdf')) && att.key) {
  zoteroLinks.push(`zotero://open-pdf/library/items/${att.key}`);
  }
  }
  if (zoteroLinks.length > 0) {
  // This overwrites the 'file' field with the working Zotero URIs
  this.add({ name: 'file', value: zoteroLinks.join(';') });
  }
  }

- another link exploiting Actions and Tags
  https://plexwave.org/blog/org-zotero-links

______________________________________________________________________

## Planned: co-located org-roam notes

**Design**: place notes next to PDFs — `~/Sync/biblio/main/KEY/KEY.org`

Required init.el changes (not yet done):

1. `(org-roam-directory "~/Sync/")` — widen from `~/Sync/notes/org-roam/`
2. `(citar-notes-source 'citar-org-roam)` — DB-based, handles nested paths
3. `(citar-notes-paths '("~/Sync/biblio/main/"))` — update path
4. Custom `citar-org-roam-capture-template` targeting `biblio/main/KEY/KEY.org`
5. `NOTER_DOCUMENT: ./filename.pdf` (relative sibling path)
6. File-level `#+ROAM_REFS: @KEY` in every note

Old notes in `~/Sync/notes/org-roam/biblio/` need one-time migration.

______________________________________________________________________

## See also

- `docs/emacs.md` — Emacs AI stack, gptel presets, MCP tools, citation
  workflow, hardware baseline, model roster, daily workflow map
- `ai-containers/README.md` — Vane, SearxNG, Khoj container setup
- `README.md` — dotfiles install, stow packages
