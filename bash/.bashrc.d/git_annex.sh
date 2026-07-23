# shellcheck shell=bash

# Git Annex Aliases

# Shortcut for 'git annex'
alias ga='git annex'
# List all Git Annex repositories
alias lg='git annex list --allrepos'
alias gaa='ga_repo_audit'
alias gam='ga_monthly'
alias gamod='ga_modernize'
alias gafm='ga_fix_missing'
alias gmaint='ga_git_maintenance'
alias gauclean='ga_unused_cleanup'
alias garawlock='ga_raw_lock'
alias garawhook='ga_raw_hook_install'
alias gabundle='ga_bundle_create'

# Git Annex Functions

_ga_require_repo() {
  if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    echo "Not inside a git repository."
    return 1
  fi
}

_ga_repo_name() {
  basename "$(git rev-parse --show-toplevel)"
}

_ga_set_remote_url() {
  local remote_name="$1"
  local remote_url="$2"

  if git remote get-url "$remote_name" > /dev/null 2>&1; then
    git remote set-url "$remote_name" "$remote_url"
  else
    git remote add "$remote_name" "$remote_url"
  fi
}

# One-page repository triage for old repositories.
ga_repo_audit() {
  _ga_require_repo || return 1

  echo "# git annex version"
  git annex version || return 1
  echo

  echo "# git annex info"
  git annex info || return 1
  echo

  echo "# git config --get annex.version"
  git config --get annex.version || true
  echo "# git config --get annex.addunlocked"
  git config --get annex.addunlocked || true
  echo "# git config --get annex.thin"
  git config --get annex.thin || true
  echo "# git config annex.sshcaching"
  git config annex.sshcaching || true
  echo "# git config annex.stalldetection"
  git config annex.stalldetection || true
  echo

  if command -v lsattr > /dev/null 2>&1; then
    echo "# lsattr -d .git/annex"
    lsattr -d .git/annex 2> /dev/null || true
    echo "# lsattr -d .git/annex/objects"
    lsattr -d .git/annex/objects 2> /dev/null || true
  else
    echo "lsattr not found; install e2fsprogs if you need filesystem attributes."
  fi
}

# Monthly operational checks.
ga_monthly() {
  _ga_require_repo || return 1
  git status --short --branch || return 1
  git annex fsck --fast --quiet || return 1
  git count-objects -vH || return 1
  git gc || return 1
  git annex find --not --copies=2
}

# Modernize an existing git-annex repository without creating a commit.
# Default-safe behavior:
# - stop if worktree/index are not clean
# - do not sync unless --sync is provided
# - do not apply chattr -C unless --apply-nocow is provided
# Usage:
#   ga_modernize [--sync] [--apply-nocow] [--force]
ga_modernize() {
  local do_sync=0
  local apply_nocow=0
  local force=0

  while [ $# -gt 0 ]; do
    case "$1" in
      --sync)
        do_sync=1
        ;;
      --apply-nocow)
        apply_nocow=1
        ;;
      --force)
        force=1
        ;;
      -h | --help)
        echo "Usage: ga_modernize [--sync] [--apply-nocow] [--force]"
        echo "  --sync         run 'git annex sync' before modernization"
        echo "  --apply-nocow  run 'chattr -C .git/annex/objects' (filesystem-dependent)"
        echo "  --force        proceed even if worktree/index are not clean"
        return 0
        ;;
      *)
        echo "Unknown option: $1"
        echo "Usage: ga_modernize [--sync] [--apply-nocow] [--force]"
        return 1
        ;;
    esac
    shift
  done

  _ga_require_repo || return 1

  if ! git annex info > /dev/null 2>&1; then
    echo "Not a git-annex repository."
    return 1
  fi

  if [ "$force" -ne 1 ]; then
    if ! git diff --quiet || ! git diff --cached --quiet; then
      echo "Stop: repository is not clean."
      echo "Commit/stash/discard changes first, or use --force."
      git status --short --branch || true
      return 1
    fi
  fi

  echo "# Current status"
  git status --short --branch || return 1

  if [ "$do_sync" -eq 1 ]; then
    echo "# Syncing Git and annex metadata/content"
    git annex sync || return 1
  else
    echo "# Skipping sync (use --sync to enable)"
  fi

  echo "# Checking annex content"
  git annex fsck --fast --quiet || return 1

  echo "# Upgrading annex repository metadata"
  git annex upgrade || return 1

  echo "# Applying annex settings"
  git config annex.addunlocked true
  git config annex.thin true
  git config annex.sshcaching true
  git config annex.stalldetection true

  local probe_rc=0
  local do_normalize=0

  # Bounded full probe: scan unlocked files with a 100s timeout.
  # Do not early-exit on first match; timeout path must remain meaningful.
  if timeout 100s bash -o pipefail -c "git annex find --unlocked | awk 'BEGIN{found=0} {found=1} END{exit found ? 0 : 1}'"; then
    do_normalize=1
  else
    probe_rc=$?
    if [ "$probe_rc" -eq 124 ]; then
      echo "# Unlocked-file probe timed out after 30s."
      echo "# This may already be a large v10/addunlocked repository."
      if [ -t 0 ]; then
        read -r -p "Proceed with lock+unlock all annexed files anyway? [y/N] " reply
        case "$reply" in
          y | Y | yes | YES)
            do_normalize=1
            ;;
          *)
            do_normalize=0
            ;;
        esac
      else
        echo "# Non-interactive shell: skipping lock+unlock normalization."
      fi
    fi
  fi

  if [ "$do_normalize" -eq 1 ]; then
    echo "# Detected (or confirmed) unlocked annexed files; normalizing with lock+unlock"
    git annex lock . || return 1
    git annex unlock . || return 1
  else
    echo "# No unlocked annexed files detected; unlocking existing annexed files"
    git annex unlock . || return 1
  fi

  if [ "$apply_nocow" -eq 1 ]; then
    if command -v lsattr > /dev/null 2>&1 && command -v chattr > /dev/null 2>&1 &&
      [ -d .git/annex/objects ] &&
      lsattr -d .git/annex/objects > /dev/null 2>&1; then
      echo "# Applying requested CoW attribute operation"
      chattr -C .git/annex/objects || return 1
    else
      echo "Skipping --apply-nocow: chattr/lsattr unavailable or objects dir inaccessible."
      return 1
    fi
  else
    echo "# Skipping CoW attribute change (use --apply-nocow to enable)"
  fi

  echo
  echo "Modernization complete; review with: git status"
  echo "No files were staged or committed."
}

# Separate cleanup for unreferenced annex content.
# Only run this after reviewing `git annex unused`.
ga_unused_cleanup() {
  _ga_require_repo || return 1
  if [ "${1:-}" != "--yes" ]; then
    echo "Refusing cleanup without explicit confirmation."
    echo "Review first with: git annex unused"
    echo "Then run: ga_unused_cleanup --yes"
    return 1
  fi
  git annex unused || return 1
  git annex dropunused all || return 1
  git annex unused --remove
}

# Run modern Git maintenance framework tasks now.
ga_git_maintenance() {
  _ga_require_repo || return 1
  git maintenance run
}

# Enable background Git maintenance scheduling.
ga_git_maintenance_start() {
  _ga_require_repo || return 1
  git maintenance start
}

# Symptom flow: file missing -> whereis -> get -> fsck.
# $1: file path
ga_fix_missing() {
  _ga_require_repo || return 1
  if [ -z "$1" ]; then
    echo "Usage: ga_fix_missing <file>"
    return 1
  fi
  git annex whereis "$1" || return 1
  git annex get "$1" || return 1
  git annex fsck "$1"
}

# Symptom flow: locked file -> unlock.
# $1: file path
ga_unlock_edit() {
  _ga_require_repo || return 1
  if [ -z "$1" ]; then
    echo "Usage: ga_unlock_edit <file>"
    return 1
  fi
  git annex unlock "$1" || return 1
  echo "Unlocked: $1"
  echo "Next: edit file, then run:"
  echo "  git commit -am \"Edit $1\""
  echo "  git annex sync --content"
}

# Quarterly restore drill for a representative path.
# $1: file or directory path
ga_restore_drill() {
  _ga_require_repo || return 1
  if [ -z "$1" ]; then
    echo "Usage: ga_restore_drill <file-or-dir>"
    return 1
  fi
  git annex whereis "$1" || return 1
  git annex get "$1" || return 1
  git annex fsck "$1"
}

# Bootstrap a new data project (for cookiecutter Data Analysis projects).
# $1: annex name (default: HOSTNAME)
# $2: optional origin URL
ga_repo_bootstrap() {
  local annex_name="${1:-${HOSTNAME:-$(hostname -s 2> /dev/null || hostname)}}"
  local origin_url="${2:-}"

  if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    git init -b main || return 1
  fi

  git annex init --version=10 "$annex_name" || return 1
  git config annex.addunlocked true
  git config annex.thin true
  git config annex.sshcaching true
  git config annex.stalldetection true

  if [ -n "$origin_url" ] && ! git remote get-url origin > /dev/null 2>&1; then
    git remote add origin "$origin_url"
  fi

  if [ -d data ]; then
    mkdir -p data/raw data/interim data/processed
  fi
}

# Configure preferred remote topology for scientific datasets:
# - origin -> GIN (annex-aware)
# - github -> GitHub mirror (annex-ignore=true)
# Usage:
#   ga_repo_bootstrap_hosted [repo_name] [github_owner] [gin_owner]
ga_repo_bootstrap_hosted() {
  local repo_name="${1:-$(_ga_repo_name)}"
  local github_owner="${2:-${GITHUB_OWNER:-darosio}}"
  local gin_owner="${3:-${GIN_OWNER:-darosio}}"

  _ga_require_repo || return 1
  ga_repo_bootstrap "${HOSTNAME:-$(hostname -s 2> /dev/null || hostname)}" || return 1
  ga_add_gin_origin "$repo_name" "$gin_owner" || return 1
  ga_add_github_mirror "$repo_name" "$github_owner" || return 1
}

# Add/update origin as GIN (and create repo if gin CLI is available).
# Usage:
#   ga_add_gin_origin [repo_name] [gin_owner] [description]
ga_add_gin_origin() {
  local repo_name="${1:-$(_ga_repo_name)}"
  local gin_owner="${2:-${GIN_OWNER:-darosio}}"
  local description="${3:-git-annex data repository}"
  local gin_url="git@gin.g-node.org:/$gin_owner/$repo_name.git"

  _ga_require_repo || return 1

  if command -v gin > /dev/null 2>&1; then
    gin create --no-clone "$repo_name" "$description" > /dev/null 2>&1 || true
  fi

  _ga_set_remote_url origin "$gin_url"
  git config remote.origin.annex-ignore false
  echo "Configured origin -> $gin_url (annex-aware)"
}

# Add/update GitHub metadata mirror and mark as annex-ignore=true.
# Usage:
#   ga_add_github_mirror [repo_name] [github_owner]
ga_add_github_mirror() {
  local repo_name="${1:-$(_ga_repo_name)}"
  local github_owner="${2:-${GITHUB_OWNER:-darosio}}"
  local github_slug="$github_owner/$repo_name"
  local github_url="git@github.com:$github_slug.git"

  _ga_require_repo || return 1

  if command -v gh > /dev/null 2>&1; then
    gh repo view "$github_slug" > /dev/null 2>&1 ||
      gh repo create "$github_slug" --private --disable-wiki --confirm > /dev/null 2>&1 || true
  fi

  _ga_set_remote_url github "$github_url"
  git config remote.github.annex-ignore true
  echo "Configured github -> $github_url (annex-ignore=true)"
}

# Add/update local storage remotes and mark them annex-ignore=true.
# Usage:
#   ga_add_storage_remotes <server_path_or_url> <backup_path_or_url>
ga_add_storage_remotes() {
  local server_url="${1:-}"
  local backup_url="${2:-}"

  _ga_require_repo || return 1
  if [ -z "$server_url" ] || [ -z "$backup_url" ]; then
    echo "Usage: ga_add_storage_remotes <server_path_or_url> <backup_path_or_url>"
    return 1
  fi

  _ga_set_remote_url server "$server_url"
  _ga_set_remote_url backup "$backup_url"
  git config remote.server.annex-ignore true
  git config remote.backup.annex-ignore true
  echo "Configured storage remotes server/backup with annex-ignore=true"
}

# Create a portable full-history cold archive bundle.
# Usage:
#   ga_bundle_create [output.bundle]
ga_bundle_create() {
  local outfile="${1:-$(_ga_repo_name)-$(date +%F).bundle}"
  _ga_require_repo || return 1
  git bundle create "$outfile" --all
  echo "Created bundle: $outfile"
}

# Enforce filesystem-level immutability for scientific raw data.
# $1: raw data directory (default: raw)
ga_raw_lock() {
  local raw_dir="${1:-raw}"
  _ga_require_repo || return 1
  if [ ! -d "$raw_dir" ]; then
    echo "Directory not found: $raw_dir"
    return 1
  fi
  chmod -R a-w "$raw_dir"
  echo "Read-only lock applied to: $raw_dir"
}

# Install a pre-commit hook that rejects staged changes under raw data path.
# Override for intentional exceptions:
#   GA_ALLOW_RAW=1 git commit -m "..."
# $1: raw data directory (default: raw)
ga_raw_hook_install() {
  local raw_dir="${1:-raw}"
  local hook_file
  _ga_require_repo || return 1
  hook_file="$(git rev-parse --git-path hooks/pre-commit)" || return 1

  cat > "$hook_file" << EOF
#!/usr/bin/env bash
set -euo pipefail

RAW_DIR="$raw_dir"

if [ "\${GA_ALLOW_RAW:-0}" = "1" ]; then
  exit 0
fi

changed=\$(git diff --cached --name-only --diff-filter=ACMRTD -- "\$RAW_DIR")
if [ -n "\$changed" ]; then
  echo "ERROR: staged changes detected in '\$RAW_DIR/' (immutable raw data policy)." >&2
  echo "Blocked files:" >&2
  echo "\$changed" >&2
  echo >&2
  echo "If this is intentional, commit with override:" >&2
  echo "  GA_ALLOW_RAW=1 git commit -m \"...\"" >&2
  exit 1
fi
EOF

  chmod +x "$hook_file"
  echo "Installed pre-commit raw-data guard for: $raw_dir"
}

# Copy directory structure to a destination
copythistree() {
  find . -type d -exec mkdir -p "$1/{}" \;
}

# Create a directory with the current date and optionally append a description
# $1: Folder in analyses
# $2: Name of the link
# $3: Optional description (e.g., adaptive_22)
lndate() {
  local dir
  dir=$1/$(date -I)
  if [ -n "$3" ]; then
    dir=${dir}_$3
  fi
  mkdir "$dir"
  ln -s "$dir" "$2"
}

# Initialize a Git repository with Git Annex
# $1: Annex configuration parameter
ga_init() {
  local annex_name="${1:-${HOSTNAME:-$(hostname -s 2> /dev/null || hostname)}}"
  git init
  git annex init "$annex_name"
  git config annex.addunlocked true
  git config annex.thin true
  git config annex.sshcaching true
  git config annex.stalldetection true

  touch README
  git add .
  git commit -m 'Initialization'
}

# Find Git Annex duplicate keys
ga_dup() {
  # shellcheck disable=SC2016
  git annex find --include '*' --format='${escaped_key} :${file}\n' |
    cut -d '-' -f 4- | sort | uniq --all-repeated=separate -w 63
}

# Remove Git Annex duplicate keys from the repository
ga_duprm() {
  ga_dup | awk -vRS= -vFS='\n' '{for (i = 2; i <= NF; i++) print $i}' |
    cut -d ':' -f 2- | xargs -d '\n' git rm
}

# Remove Git Annex duplicate keys from the repository and stage the removal
ga_duprmgit() {
  ga_duprm
  git add -u
}
