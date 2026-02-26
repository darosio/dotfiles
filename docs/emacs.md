# Emacs

## Package Management

Emacs packages are managed with
[straight.el](https://github.com/radian-software/straight.el) and pinned via
`emacs/.emacs.d/straight/versions/default.el`.

### Upgrading Packages

**Locally:**

```bash
make upgrade-emacs    # pull → normalize → freeze → thaw → check → smoke tests
# review changes to emacs/.emacs.d/straight/versions/default.el
git add -A && git commit -m "bump(emacs): update straight.el packages"
git push
```

**Via CI:** trigger the *Emacs CI* workflow manually (`workflow_dispatch`). It
runs `make upgrade-emacs` and creates a PR if versions changed.

**After merging a CI-created PR:**

```bash
git pull
make thaw-emacs       # checkout pinned commits + rebuild
```

### Package Management Commands

```text
straight-remove-unused-repos
straight-prune-build
straight-pull-all
straight-check-all
straight-freeze-versions
```

## Keybindings

Keybindings reserved for users: `C-c <letter>` and `F5`–`F9`.

## Configured Packages

### Core

- **use-package**, **straight**, **async**, **which-key**

### Completion & Search

- **vertico**, **consult**, **embark**, **corfu**, **cape**, **orderless**
- **yasnippet** (`M-s y`)
- **recoll** (counsel recoll)
- **consult-project-extra**

### Navigation

- **imenu** (`F9`, `C-F9`)
- **ace-window**

### Org Mode

- **org-roam**, **consult-notes** (`M-s M-n`), **org-modern**
- **org-super-agenda**, **org-ql** — agenda views with daily and weekly review
  - Shopping and weight captures
- **org-babel** — graphviz, plantuml, ditaa, Jupyter
- **org-noter** — precise insertion
- **org-capture**, **org-download**, **org-cliplink**, **org-pdftools**,
  **org-autolist**, **org-indent**
- **org-lint**
- **org-compat** — mpv links
- Export: **ox-rst**, **ox-pandoc**, **ox-twbs**, **ox-beamer**, **ox-md**,
  **ox-koma-letter**

### Email

- **mu4e** and calendar
- **org-mime** for mu4e compose
- `C-c o a` — mu4e-compose-attach-captured-message

### Notes

- **deft** — native insert mode; can create new `./proj/file`

### Git

- **magit**, **magit-todos**

### Development

- **eglot**, **flymake**, **envrc** (direnv support)
- **python-pytest**
- **apheleia** — in-place formatting (black, prettier)
- **numpydoc**

### Editing

- **smartparens** — `C-backspace`, `H-b`, `H-f`, `H-h`, `H-d`, `H-k`, `H-t`,
  `H-()`, `H-{}`
- **expand-region** (`C-=`)
- **aggressive-indent**, **electric-indent**
- **hideshow** — code folding
- **move-text**, **unfill**
- **visual-fill-column** — also for distraction-free writing

### LaTeX

- **auctex**, **cdlatex**

### AI

- **gptel**

### UI & Window Management

- **doom-modeline**
- Text scale increase/decrease (`C-x C-0`)
- Maximize window (`H-m`) — `prot/window-single-toggle`

### Misc

- **password-store**, **recentf**, **hl-todo**, **visual-regexp**
- **nov** — EPUB reader
- **csv-mode** (`C-c C-a`)
- **gtklp** — CUPS printing
- **citar** — citation management
- **calfw-org** — calendar

## Vanilla Emacs Use Cases

- **PIM:** GTD (org, super-agenda), email (mu4e), notes (deft)
- **Bibliography:** importing/searching (org-ref, doi), managing .bib
  (bibtex, ivy-bibtex), notes (org-noter, ORB)
- **Writing:** LaTeX, pandoc, org-ref
- **Git:** magit
