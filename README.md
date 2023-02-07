# Repository for $HOME/.dotfiles

Version: "3.1.0"

## Requirements

1. GNU stow 

## Usage

to create symlink into $HOME folder:

	cd ~/.dotfile
	stow package-version 
	
for some packages a folder must be created first, e.g.:

    mkdir ~/.vim
    then ~/.vim/bundle/Vundle.vim

pick package-version as needed.

Plan to use machine-specific git branches.

I location of .dotfiles is moved please first:

stow -t ~/ misc
stoe -t ~/ bash

Remember to create folders and linkers where needed.

## submodule problem
I had once and solved the detached head problem following:
https://stackoverflow.com/questions/18770545/why-is-my-git-submodule-head-detached-from-master

git branch -u refs/remotes/origin/master master
git co master

check .gitmodule .git/config

or:
 $ cd <submodule-path>
    $ git checkout <branch>
    $ cd <parent-repo-path>
    # <submodule-path> is here path releative to parent repo root
    # without starting path separator
    $ git config -f .gitmodules submodule.<submodule-path>.branch <branch>

## add new submodules
    $ git submodule add -b <branch> <repository> [<submodule-path>]
    $ git submodule update --remote

## remove submodules


    Delete the relevant section from the .gitmodules file.
    Stage the .gitmodules changes git add .gitmodules
    Delete the relevant section from .git/config.
    Run git rm --cached path_to_submodule (no trailing slash).
    Run rm -rf .git/modules/path_to_submodule
    Commit git commit -m "Removed submodule <name>"
    Delete the now untracked submodule files
    rm -rf path_to_submodule


## emacs

straight external requirement:
- watchexec

straight-remove-unused-repos
straight-prune-build
straight-pull-all
straight-check-all
straight-freeze-versions

Keybinding reserved to users are: C-c <letter> and F5 to F9.

### Configured packages include:
- use-package with async
- which-key
- modalka
- ivy counsel swiper; overload `C-h f`, `C-h v`, `C-s` and `M-x`
- avy (move around) and smex (remember places)
- imenu with `F9` and `C-F9` overloading
- mu4e and calendar https://jherrlin.github.io/posts/emacs-mu4e/
- deft with native insert mode; can create new ./proj/file
- open file in emacs with external application: `SPC f f` `M-o x`
- completion with company and (ivy-)yasnippet `SPC i s` also for longer org
  easy template; dropped: auto-yasnippet and yatemplate
- projectile, magit, auto-completion, counsel recoll
- org babel, graphviz, plantuml, markdown, criticmarkup
- zeal-at-point
- text scale increase/decrease `C-x C-0`
- typo mode `F14 t T`
- gtklp for printing using cups
- expand-region `C-=`
- dired
- smartparens `C-backspace` `H-b` `H-f` `H-h` `H-d` `H-k` `H-t` `H-()` `H-{}`.
- hideshow for folding.
- electric and aggressive indent.
- visual fill column also for distraction free behavior.
- slack with a hydra.
- nov.
- Maximize window `H-m` . prot/window-single-toggle.
- ace-win.
- calendars with calfw-org
- org
  - org-lint
  - org-bullet org-attach org-download org-cliplink org-pdftools org-autolist org-indent
  - ox-rst -pandoc -twbs -beamer -md -koma-letter
  - org-compat for mpv links
  - org-agenda views with daily and weekly review using org-super-agenda and org-ql.
   - shopping and weight captures
   - plantuml graphviz ditaa Jupyter
- org-mime for mu4e compose.
- auctex cdlatex
- proj todos can go with either org-projectile or magit-todos.
- python
  - py-isort manually
  - numydoc
  - poetry
  - pyenv (pyvenv)?
- ORB with org-noter with precise insertion
- csv-mode `C-c C-a`.
- `C-c o a` mu4e-compose-attach-captured-message.



## Vanilla emacs
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

