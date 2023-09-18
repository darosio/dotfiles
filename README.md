# Repository for $HOME/.dotfiles

Version: "3.7.0"

## Requirements

1. GNU stow
2. watchexec – for emacs package management with straight
3. hatch – install using `pipx install hatch`

Initialize pre-commit hooks:

    hatch run init

When bump version

    hatch run bump
    git push
    git push --tags

## Usage

To create symlink into $HOME:

    cd ~/workspace/dotfiles # or new location of dotfiles/
    stow package-version

when target is for example =/home use:

    stow -t /home mr

for some packages a folder must be created first, e.g.:

    mkdir ~/.vim
    then ~/.vim/bundle/Vundle.vim

pick package-version as needed.

Better practice is to use specialized scripts e.g.:

    gh.stow.sh
    psd.stow.sh
    emacs.stow.sh
    rclone.stow.sh
    recoll.stow.sh
    goldendict.stow.sh

Plan to use machine-specific git branches is on hold.

Update using `mr`:

    cd workspace/repo
    mr register

## add new submodules

    git submodule add -b <branch> <repository> [<submodule-path>]
    git submodule update --remote

## remove submodules

    Delete the relevant section from the .gitmodules file.
    Stage the .gitmodules changes git add .gitmodules
    Delete the relevant section from .git/config.
    Run git rm --cached path_to_submodule (no trailing slash).
    Run rm -rf .git/modules/path_to_submodule
    Commit git commit -m "Removed submodule <name>"
    Delete the now untracked submodule files
    rm -rf path_to_submodule

## submodule problem

I had once and solved the detached head problem following:
<https://stackoverflow.com/questions/18770545/why-is-my-git-submodule-head-detached-from-master>

    git branch -u refs/remotes/origin/master master
    git co master

check .gitmodule .git/config or:

    cd <submodule-path>
    git checkout <branch>
    cd <parent-repo-path> # relative to parent repo root without starting path separator
    git config -f .gitmodules submodule.<submodule-path>.branch <branch>

## Development

Commit following commitizen but consider this is a repository of configurations
e.g. when updating emacs packages

    feat(emacs): Update packages

what would normally called `build: Update dependencies`.

Remember:

    gh pr merge --squash --delete-branch -t “feat(emacs): …”

## Applications [optional] requirements

### Ranger

- xls2csv
- xlsx2csv
- feh
- reformime from mailcap

## emacs

TODO: [notes](/home/dan/Sync/notes/arch/emacs.org "emacs")

Keybinding reserved to users are: C-c \<letter\> and F5 to F9.

Package management useful commands:

    straight-remove-unused-repos
    straight-prune-build
    straight-pull-all
    straight-check-all
    straight-freeze-versions

### To check

- mu4e
  - send and get email using mbsync
  - Reply to all with duck hack
- org-roam-protocol
- consult-notes
- lsp
  - go to definition
  - hatch direnv …

### List of configured packages

- Use-package, straight, async and which-key.
- Imenu with `F9` and `C-F9` overloading.
- Mu4e and calendar <https://jherrlin.github.io/posts/emacs-mu4e/>.
- Deft with native insert mode; can create new ./proj/file.
- Org-roam-note and consult-note e.g. `M-s M-n`.
- Counsel recoll.
- Completion with vertico and yasnippet `M-s y`.
- Consult-project-extra, magit and magit-todos.
- Org babel, graphviz, plantuml, markdown, criticmarkup and typo mode `F14 t T`.
- Text scale increase/decrease `C-x C-0`.
- Gtklp for printing using cups.
- Expand-region `C-=`.
- Smartparens `C-backspace` `H-b` `H-f` `H-h` `H-d` `H-k` `H-t` `H-()` `H-{}`
  etc.
- Hideshow for folding..
- Electric and aggressive indent.
- Visual fill column also for distraction free behavior.
- Slack with a hydra.
- nov.
- Maximize window `H-m` . prot/window-single-toggle.
- ace-win.
- calendars with calfw-org
- org
  - org-lint
  - org-bullet org-attach org-download org-cliplink org-pdftools org-autolist
    org-indent
  - ox-rst -pandoc -twbs -beamer -md -koma-letter
  - org-compat for mpv links
  - org-agenda views with daily and weekly review using org-super-agenda and
    org-ql.
    - shopping and weight captures
    - plantuml graphviz ditaa Jupyter
- org-mime for mu4e compose.
- auctex cdlatex
- python
  - py-isort manually
  - numydoc
  - envrc to support direnv
- org-noter with precise insertion
- csv-mode `C-c C-a`.
- `C-c o a` mu4e-compose-attach-captured-message.
- Apheleia in place of blacken support black and prettier.

#### Vanilla emacs

Using emacs as editor serving also the following purposes:

- PIM
  - GTD (org, superagenda, …)
  - email (mu4e)
  - notes (deft)
- bibliography manager
  - importing and searching (org-ref, doi, web XXX)
  - managing .bib db (bibtex, ivy-bibtex, org-ref)
  - notes (org-noter, ORB)
- writing (latex, pandoc, org-ref)
- git (magit)
