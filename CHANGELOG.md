<!-- markdownlint-configure-file { "MD013": { "line_length": 90 } } -->
<!-- markdownlint-configure-file { "MD024": { "allow_different_nesting": true } } -->

# Changelog

All notable changes to my dotfiles repo will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

Releases will start from 2022.1.

## 3.15.0 (2024-04-10)

### Feat

- Drop notmuch
- Apheleia adopt ruff-lsp

### Build

- Update deps
- **deps-dev**: bump ruff from 0.3.4 to 0.3.5 (#60)
- **deps-dev**: bump commitizen from 3.20.0 to 3.21.3 (#59)

## 3.14.1 (2024-03-26)

### Fix

- lsp-mode by reverting previous update

### Build

- Update deps
- **deps-dev**: bump pre-commit from 3.6.2 to 3.7.0 (#58)

## 3.14.0 (2024-03-25)

### Feat

- **emacs**: Update packages

### Build

- **deps-dev**: bump ruff from 0.3.3 to 0.3.4 (#57)
- **deps-dev**: bump commitizen from 3.18.4 to 3.20.0 (#56)
- **deps-dev**: bump ruff from 0.3.2 to 0.3.3 (#55)
- **deps-dev**: bump commitizen from 3.18.3 to 3.18.4 (#54)
- **deps-dev**: bump commitizen from 3.18.2 to 3.18.3 (#53)
- **deps-dev**: bump ruff from 0.3.1 to 0.3.2 (#52)
- **deps-dev**: bump commitizen from 3.17.0 to 3.18.2 (#51)
- **deps-dev**: bump ruff from 0.3.0 to 0.3.1 (#49)
- **deps-dev**: bump commitizen from 3.16.0 to 3.17.0 (#48)
- **deps-dev**: bump ruff from 0.2.2 to 0.3.0 (#47)

## 3.13.0 (2024-02-29)

### Feat

- **emacs**: Update packages
- **emacs**: Update mu4e 1.12

### Fix

- **emacs**: Update to mu-1.12 replace-duck-emails

### Build

- **deps-dev**: bump commitizen from 3.14.1 to 3.16.0 (#46)
- **deps-dev**: bump ruff from 0.2.1 to 0.2.2 (#44)
- **deps-dev**: bump pre-commit from 3.6.1 to 3.6.2 (#43)
- **deps-dev**: bump pre-commit from 3.6.0 to 3.6.1 (#42)
- **deps-dev**: bump ruff from 0.2.0 to 0.2.1 (#41)
- **deps-dev**: bump commitizen from 3.14.0 to 3.14.1 (#40)
- **deps-dev**: bump ruff from 0.1.14 to 0.2.0 (#39)
- **deps-dev**: bump commitizen from 3.13.0 to 3.14.0 (#38)
- **deps-dev**: bump ruff from 0.1.13 to 0.1.14 (#36)
- **deps-dev**: bump ruff from 0.1.12 to 0.1.13 (#35)
- **deps-dev**: bump ruff from 0.1.11 to 0.1.12 (#34)
- **deps-dev**: bump ruff from 0.1.9 to 0.1.11 (#33)

### chore

- Update xrandr cmd for sterzing

## 3.12.0 (2024-01-02)

### Feat

- **emacs**: Update packages
- **git**: Add `remove-ipynb-output` filter
- Add xr and xr1 for 2 monitor @fbk
- **emacs**: Update packages
- **emacs**: Update packages
- Add lsfonts

### Fix

- **emacs**: Folding of python with hideshow
- **emacs**: org captures

### Build

- **deps-dev**: bump ruff from 0.1.8 to 0.1.9 (#32)
- **deps-dev**: bump ruff from 0.1.7 to 0.1.8 (#31)
- **deps-dev**: bump pre-commit from 3.5.0 to 3.6.0 (#30)
- **deps-dev**: bump ruff from 0.1.6 to 0.1.7 (#29)
- **deps-dev**: bump commitizen from 3.12.0 to 3.13.0 (#28)
- **deps-dev**: bump ruff from 0.1.5 to 0.1.6 (#27)
- **deps-dev**: bump ruff from 0.1.4 to 0.1.5 (#26)
- **deps-dev**: bump ruff from 0.1.3 to 0.1.4 (#25)

### chore

- Adopt jupyter-lab
- **emacs**: Always use straight.el not package.el with use-package

## 3.10.0 (2023-10-31)

### Feat

- **emacs**: Update packages and fix a magit/straight issue

### Fix

- **emacs**: chatgpt and dall-e org-mode babel
- msmtp and sendmail

### Build

- **deps-dev**: bump ruff from 0.1.2 to 0.1.3 (#24)

## 3.9.0 (2023-10-27)

### Feat

- **emacs**: Update packages

### Build

- **deps-dev**: bump ruff from 0.1.1 to 0.1.2 (#23)
- **deps-dev**: bump ruff from 0.1.0 to 0.1.1 (#22)
- **deps-dev**: bump commitizen from 3.10.1 to 3.12.0 (#21)
- **deps-dev**: bump ruff from 0.0.292 to 0.1.0 (#20)
- **deps-dev**: bump commitizen from 3.10.0 to 3.10.1 (#19)
- **deps-dev**: bump pre-commit from 3.4.0 to 3.5.0 (#18)

## 3.8.0 (2023-10-12)

### Feat

- **emacs**: Update straight all packages
- **emacs**: mastodon and twitter
- rmlintt searches include (-r) hidden files

### Fix

- Add chatgpt-shell

### Build

- **deps-dev**: bump ruff from 0.0.291 to 0.0.292 (#17)
- **deps-dev**: bump ruff from 0.0.290 to 0.0.291 (#16)
- **deps-dev**: bump commitizen from 3.9.1 to 3.10.0 (#15)
- **deps-dev**: bump commitizen from 3.9.0 to 3.9.1 (#14)
- **deps-dev**: bump commitizen from 3.8.2 to 3.9.0 (#13)
- **deps-dev**: bump ruff from 0.0.289 to 0.0.290 (#12)

### Refactor

- Add script also for RAID essential into root
- **emacs**: Remove org-bullets; don’t indent after heading

### chore

- **vale**: Add joblint and alex to install (`vale sync`)

## 3.7.0 (2023-09-18)

### Feat

- **emacs**: Update straight all packages
- Add `ee` `ea` `rmlintt` aliases
- **ranger**: Enable pdf preview as image
- **rclone**: Add Onedrive of CNR
- **emacs**: Update to 29.1 pin org to 9.5.5 to fix org-roam issue
- Add md2mb mu_tags2notmuch and notmuch in emacs
- Drop courier-maildrop reformime in favor of mu or python scripts
- separate system and user sendmail with OpenSMTPD and msmtp
- Fix recoll.stow
- gvim.stow
- smartd without root .msmtprc
- New emacs and ranger stow.sh
- **systemd**: failure notification
- add python-version=system
- **emacs**: Update emacs from 28.2 to 29.1
- **git**: pull.rebase false
- **emacs**: Add lsp-treemacs
- **emacs**: Bind magit-todos to `C-c g 2` `C-c 2 3`
- Add mu_tags_all_rg because faster than mu_tags with SSD
- Add tree_size function
- **emacs**: Update consul-notes and config consult-ripgrep
- **ranger**: A-f fzf_select Refactor bashrc functions and aliases
- **emacs**: update packages

### Fix

- gh and direnv
- 2root.smartd.sh
- gvim.stow
- emacs.stow
- urxvt
- smartdnotify
- direnvrc for hatch
- box.com in rclone
- **emacs**: mu4e tag untag

### Perf

- further improvements
- update initial installation
- **mu_tags**: Use mu find "" to speedup tags lists

### Build

- Fix `hatch run bump`
- **deps-dev**: bump ruff from 0.0.288 to 0.0.289 (#11)
- **deps-dev**: bump ruff from 0.0.287 to 0.0.288 (#10)
- **deps-dev**: bump commitizen from 3.8.0 to 3.8.2 (#9)
- **deps-dev**: bump commitizen from 3.7.0 to 3.8.0 (#8)
- **deps-dev**: bump ruff from 0.0.286 to 0.0.287 (#7)
- **deps-dev**: bump pre-commit from 3.3.3 to 3.4.0 (#6)
- **deps-dev**: bump commitizen from 3.6.0 to 3.7.0 (#5)
- **deps-dev**: bump ruff from 0.0.285 to 0.0.286 (#4)
- **deps-dev**: bump ruff from 0.0.284 to 0.0.285 (#3)
- **deps-dev**: bump ruff from 0.0.282 to 0.0.284 (#2)
- **emacs**: Update some packages and ps-print

### Refactor

- **emacs**: aur/ttf-symbola is not needed
- mr.stow
- sshd
- smartd
- cleanup

### chore

- **mr**: Add caiman example
- More `examples`
- init scripts bash misc
- remove old systemd-email
- **emacs**: removed few commands after consult dropped them
- Open file after sf recoll search
- **ranger**: change some rifle default
- **emacs**: Drop all-the-icons for nerd-icons in doom-modeline

## 3.6.0 (2023-06-14)

### Feat

- Add delta to gitconfig and (e)diff
- **emacs**: Add gptchat-shell
- **emacs**: Accommodate update of python from 3.10 to 3.11
- **emacs**: Disable typo-mode but have it global with C-c 8
- **emacs**: Review ox-latex

### Fix

- **emacs**: mu4e missing msg because not updating index
- Update to default python 3.11
- **emacs**: ESS by restoring process-connection-type t
- **emacs**: flycheck and is-daemon for mu4e
- mimeapps.list

### Docs

- Lint CHANGELOG.md with the correct exceptions for markdownlint

### Style

- **emacs**: untabify

### Build

- Upgrade pre-commit removing black for docs
- **emacs**: Update vertico consult-notes org-noter and deps

### Refactor

- **emacs**: mu4e after CNR2office365

### chore

- **emacs**: consult ripgrep-args
- **alias**: xmm for xmodmap
- **emacs**: Bind org-lint to `M-g e o`

## 3.5.0 (2023-04-11)

### Feat

- Add proselint and vale configurations
- **emacs**: Fix (fly)check for prose
- **emacs**: Clean writing session
- **emacs**: Remove char-menu and avy (dep of ace-window eval-in-repl)
- **emacs**: Add consult-flyspell `M-g s` and refactor
- **emacs**: Buffer hydra and refactor themes

### Fix

- **emacs**: consult-notes
- **emacs**: apheleia prettier for markdown
- **emacs**: Alias `e`
- **emacs**: org-noter-pdftools. Refactor writing session
- **emacs**: keybinding

### Docs

- Lint README.md with markdownlint-cli

### Refactor

- **emacs**: Organize yasnippet and other packages
- **emacs**: ace-window `C-’` `C-“`
- **emacs**: dired -x wdired
- **emacs**: `C-x b` consult-buffer
- **emacs**: mu4e and bind `C-c C-o` org-msg

### chore

- **emacs**: Keybinding `C-N`and `C-P` to navigate vertico group

## 3.4.2 (2023-04-04)

### Fix

- **emacs**: org -roam -protocol
- **emacs**: use-package-autoload-keymap might not be defined at runtime
- **emacs**: pdf-tools demand

### chore

- **emacs**: gscholar-bibtex `M-s C-b`

## 3.4.1 (2023-04-03)

### Fix

- **emacs**: Update packages init.el and readme

## 3.4.0 (2023-04-03)

### Feat

- **emacs**: removed modalka refactor keybinding

### Fix

- **emacs**: daemon demanding packages

## 3.3.0 (2023-04-02)

### Feat

- **emacs**: Drop ranger Fix windows Add lpr Refactor init.el
- **emacs**: Update mu Adjust consult preview Hook citar-capf
- **emacs**: Drop org-super-agenda and org-ql
- **emacs**: Reorganize agendas and include org-roam in refile targets
- **emacs**: consult notes keybindings like `M-s n` and fix and cleanup
- **emacs**: drop sphinx-doc python-docstring-mode (hyperscheduler)
- **ranger**: Preview eml using maildrop `reformime`
- Add syncthing, refactor mrconfigs and cleanup
- **ranger**: Update config files
- **emacs**: apheleia in place of black, pretty-js etc.

### Fix

- **emacs**: gcal oauth2 with captures
- **ranger**: org file opening
- **memory**: As alias see https://github.com/pixelb/scripts
- **mr|bash**: mr config and bashrc
- environment.d for rclone, emacs, etc.
- **gh**: Auth with token

### Docs

- Update readme

### Style

- Drop yamllint in favor of prettier

### Refactor

- **emacs**: init files with ChatGPT; `M-s c` to open ref notes
- Adopt ruff black blacken-docs in all scripts

## 3.2.1 (2023-02-09)

### Fix

- detect secrets

### Style

- some linting

### Refactor

- many shell scripts

## 3.2.0 (2023-02-08)

### Feat

- cz section and pre-commit deps

## 3.11.0 (2023-11-13)

### Feat

- **emacs**: Update packages
- **emacs**: Update packages
- Add lsfonts

### Build

- **deps-dev**: bump ruff from 0.1.4 to 0.1.5 (#26)
- **deps-dev**: bump ruff from 0.1.3 to 0.1.4 (#25)

### chore

- **emacs**: Always use straight.el not package.el with use-package

## 3.10.0 (2023-10-31)

### Feat

- **emacs**: Update packages and fix a magit/straight issue

### Fix

- **emacs**: chatgpt and dall-e org-mode babel
- msmtp and sendmail

### Build

- **deps-dev**: bump ruff from 0.1.2 to 0.1.3 (#24)

## 3.9.0 (2023-10-27)

### Feat

- **emacs**: Update packages

### Build

- **deps-dev**: bump commitizen from 3.10.0 to 3.10.1 (#19)
- **deps-dev**: bump pre-commit from 3.4.0 to 3.5.0 (#18)

## 3.8.0 (2023-10-12)

### Feat

- **emacs**: Update straight all packages
- **emacs**: mastodon and twitter
- rmlintt searches include (-r) hidden files

### Fix

- Add chatgpt-shell

### Build

- **deps-dev**: bump ruff from 0.0.291 to 0.0.292 (#17)
- **deps-dev**: bump ruff from 0.0.290 to 0.0.291 (#16)
- **deps-dev**: bump commitizen from 3.9.1 to 3.10.0 (#15)
- **deps-dev**: bump commitizen from 3.9.0 to 3.9.1 (#14)
- **deps-dev**: bump commitizen from 3.8.2 to 3.9.0 (#13)
- **deps-dev**: bump ruff from 0.0.289 to 0.0.290 (#12)
- Fix `hatch run bump`

### Refactor

- Add script also for RAID essential into root
- **emacs**: Remove org-bullets; don’t indent after heading

### chore

- **vale**: Add joblint and alex to install (`vale sync`)

## 3.7.0 (2023-09-18)

### Feat

- **emacs**: Update straight all packages
- Add `ee` `ea` `rmlintt` aliases
- **ranger**: Enable pdf preview as image
- **rclone**: Add Onedrive of CNR
- **emacs**: Update to 29.1 pin org to 9.5.5 to fix org-roam issue
- Add md2mb mu_tags2notmuch and notmuch in emacs
- Drop courier-maildrop reformime in favor of mu or python scripts
- separate system and user sendmail with OpenSMTPD and msmtp
- Fix recoll.stow
- gvim.stow
- smartd without root .msmtprc
- New emacs and ranger stow.sh
- **systemd**: failure notification
- add python-version=system
- **emacs**: Update emacs from 28.2 to 29.1
- **git**: pull.rebase false
- **emacs**: Add lsp-treemacs
- **emacs**: Bind magit-todos to `C-c g 2` `C-c 2 3`
- Add mu_tags_all_rg because faster than mu_tags with SSD
- Add tree_size function
- **emacs**: Update consul-notes and config consult-ripgrep
- **ranger**: A-f fzf_select Refactor bashrc functions and aliases
- **emacs**: update packages

### Fix

- gh and direnv
- 2root.smartd.sh
- gvim.stow
- emacs.stow
- urxvt
- smartdnotify
- direnvrc for hatch
- box.com in rclone
- **emacs**: mu4e tag untag

### Perf

- further improvements
- update initial installation
- **mu_tags**: Use mu find "" to speedup tags lists

### Build

- Fix `hatch run bump`
- **deps-dev**: bump ruff from 0.0.288 to 0.0.289 (#11)
- **deps-dev**: bump ruff from 0.0.287 to 0.0.288 (#10)
- **deps-dev**: bump commitizen from 3.8.0 to 3.8.2 (#9)
- **deps-dev**: bump commitizen from 3.7.0 to 3.8.0 (#8)
- **deps-dev**: bump ruff from 0.0.286 to 0.0.287 (#7)
- **deps-dev**: bump pre-commit from 3.3.3 to 3.4.0 (#6)
- **deps-dev**: bump commitizen from 3.6.0 to 3.7.0 (#5)
- **deps-dev**: bump ruff from 0.0.285 to 0.0.286 (#4)
- **deps-dev**: bump ruff from 0.0.284 to 0.0.285 (#3)
- **deps-dev**: bump ruff from 0.0.282 to 0.0.284 (#2)
- **emacs**: Update some packages and ps-print

### Refactor

- **emacs**: aur/ttf-symbola is not needed
- mr.stow
- sshd
- smartd
- cleanup

### chore

- **mr**: Add caiman example
- More `examples`
- init scripts bash misc
- remove old systemd-email
- **emacs**: removed few commands after consult dropped them
- Open file after sf recoll search
- **ranger**: change some rifle default
- **emacs**: Drop all-the-icons for nerd-icons in doom-modeline

## 3.6.0 (2023-06-14)

### Feat

- Add delta to gitconfig and (e)diff
- **emacs**: Add gptchat-shell
- **emacs**: Accommodate update of python from 3.10 to 3.11
- **emacs**: Disable typo-mode but have it global with C-c 8
- **emacs**: Review ox-latex

### Fix

- **emacs**: mu4e missing msg because not updating index
- Update to default python 3.11
- **emacs**: ESS by restoring process-connection-type t
- **emacs**: flycheck and is-daemon for mu4e
- mimeapps.list

### Docs

- Lint CHANGELOG.md with the correct exceptions for markdownlint

### Style

- **emacs**: untabify

### Build

- Upgrade pre-commit removing black for docs
- **emacs**: Update vertico consult-notes org-noter and deps

### Refactor

- **emacs**: mu4e after CNR2office365

### chore

- **emacs**: consult ripgrep-args
- **alias**: xmm for xmodmap
- **emacs**: Bind org-lint to `M-g e o`

## 3.5.0 (2023-04-11)

### Feat

- Add proselint and vale configurations
- **emacs**: Fix (fly)check for prose
- **emacs**: Clean writing session
- **emacs**: Remove char-menu and avy (dep of ace-window eval-in-repl)
- **emacs**: Add consult-flyspell `M-g s` and refactor
- **emacs**: Buffer hydra and refactor themes

### Fix

- **emacs**: consult-notes
- **emacs**: apheleia prettier for markdown
- **emacs**: Alias `e`
- **emacs**: org-noter-pdftools. Refactor writing session
- **emacs**: keybinding

### Docs

- Lint README.md with markdownlint-cli

### Refactor

- **emacs**: Organize yasnippet and other packages
- **emacs**: ace-window `C-’` `C-“`
- **emacs**: dired -x wdired
- **emacs**: `C-x b` consult-buffer
- **emacs**: mu4e and bind `C-c \C-o` org-msg

### chore

- **emacs**: Keybinding `C-N`and `C-P` to navigate vertico group

## 3.4.2 (2023-04-04)

### Fix

- **emacs**: org -roam -protocol
- **emacs**: use-package-autoload-keymap might not be defined at runtime
- **emacs**: pdf-tools demand

### chore

- **emacs**: gscholar-bibtex `M-s C-b`

## 3.4.1 (2023-04-03)

### Fix

- **emacs**: Update packages init.el and readme

## 3.4.0 (2023-04-03)

### Feat

- **emacs**: removed modalka refactor keybinding

### Fix

- **emacs**: daemon demanding packages

## 3.3.0 (2023-04-02)

### Feat

- **emacs**: Drop ranger Fix windows Add lpr Refactor init.el
- **emacs**: Update mu Adjust consult preview Hook citar-capf
- **emacs**: Drop org-super-agenda and org-ql
- **emacs**: Reorganize agendas and include org-roam in refile targets
- **emacs**: consult notes keybindings like `M-s n` and fix and cleanup
- **emacs**: drop sphinx-doc python-docstring-mode (hyperscheduler)
- **ranger**: Preview eml using maildrop `reformime`
- Add syncthing, refactor mrconfigs and cleanup
- **ranger**: Update config files
- **emacs**: apheleia in place of black, pretty-js etc.

### Fix

- **emacs**: gcal oauth2 with captures
- **ranger**: org file opening
- **memory**: As alias see <https://github.com/pixelb/scripts>
- **mr|bash**: mr config and bashrc
- environment.d for rclone, emacs, etc.
- **gh**: Auth with token

### Docs

- Update readme

### Style

- Drop yamllint in favor of prettier

### Refactor

- **emacs**: init files with ChatGPT; `M-s c` to open ref notes
- Adopt ruff black blacken-docs in all scripts

## 3.2.1 (2023-02-09)

### Fix

- detect secrets

### Style

- some linting

### Refactor

- many shell scripts

## 3.2.0 (2023-02-08)

### Feat

- cz section and pre-commit deps

## [cleaning] 3.1.0 - 2023-02-07

### Added

- BioSyntax into cleaned vimrc.
- (venv) hatch.

#### emacs

- links in ~/.emacs.d from ~/Sync/.emacs
- emacs.stow.sh
- consult-org-roam and consul-notes (I keep deft `M-s C-n`).
- tzc binding with `C-x T`.
- insert-emoji with `C-c E`.
- dna-mode and pdb-mode `C-c t m`.
- abbrev-mode `C-c t a`.
- ox-hugo.
- comint dynamic-complete-filename with `C-c <tab>`.
- languagetool and lsp-ltex to test.
- envrc (also tried direnv and bufferenv).

### Removed

- shorewall
- xfce
- vanilla-emacs

#### emacs

- synosaurus and goldendict (I picked sdvc).
- xeft and notdeft.
- themes like doom, zenburn, plan9, and espresso.
- visual-regexp.
- key-chord.
- kind-icon.
- paradox.
- rainbow-delimiters.
- typit.
- exec-path-from-shell.
- lsp-treemacs.
- pip-requirements.
- pyenv-mode.
- pyvenv.
- poetry.

### Changed

- recollindex do not skim ~/Sync/notes anymore.
- fix: pass and mu4e PASSWORDSTORE .config/environment.d (was pam_environment).
- fix: goldendict .config/environment.d.
- (git) defaultBranch = main.
- remove images from pdf with pdf_myreduce2.
- organization between .functions and .aliases adding `wifiscan`.
- snapshots into tmp PrtSc.
- (direnv) support for poetry, pdm (convenient `pdm search`) and hatch.

#### emacs

- less `<f14>` binding in favor of `M-s` and `C-x`.
- emacs-jupyter uses python3 in current venv.

## [vertico] 3.0.0 - 2022-10-24

### Added

- recoll.stow bash to make sure folders either exist or get created.
- direnv.
- `lg` alias to git annex list --allrepos
- fig for `htop`.

#### emacs

- early\-init.el.
- Completion with vertico, marginalia, embark (configured with which-key),
  consult, orderless and corfu.
- Fuzzy search with affe.
- citar.
- wgrep.
- rg with '--hidden' args.
- extra (prog) modes.
- consult-completion-in-region and corfu(-doc) manually triggered.
- lsp-mode (hook lsp activation after poetry if pyproject.toml exists). Remember
  to install (using poetry) python-lsp-server and lsp-mypy.
- treemacs
- devdocs.
- experimenting xeft notdeft and consult-notes.
- org-roam-ui.
- vterm.
- pocket-reader.

### Removed

- user-requirements files as pip-tools is superseded by poetry and pipx.
- Config of pam_environment.

#### emacs

- Completions with ivy, swiper counsel.
- fzf.
- ivy-bibtex.
- flyspell-lazy.
- company and companion.
- company-jedi.
- org-ref (using citar).
- package.el conflicting with straight.

### Changed

- updated submodule revealjs.
- fix rclone.
- add g100 (cineca) in ssh.
- add box to rclone.
- add alias `lg` for git-annex –all-repos.
- mbsync moved outside of ~/Sync and link to archive(s) HUGE improve avoiding
  many sync conflicts.
- Update `mr` config.
- Data organization in vigolana sync to sterzing exclude MM/.

#### emacs

- all the icons.
- fix emoji.
- fix spacemacs theme.
- fix org store link.
- add poetry tracking mode `f14 t P`.
- (root) emacs is emacs-vanilla.

## [straight] 2.1.0- 2022-01-30

### Added

- New file ~/.function for function command in bash like `rga-fzf` for
  rip-grep-all.
- smartd.conf in 4root.
- systemd/network/\*.network files.
- wpa_supplicant for wired eduroam.
- gmail-isync.

#### emacs

- Staight for much improved bootstrap. (Does package order now matter?)
- tzc package (timezone conversions).
- org-latex classes "apa6".
- flycheck-vale.
- ~/.local/bin/em.
- ~/.config/systemd/user/emacsu.service.
- ~/.local/share/applications/org-protocol.desktop.
- Straight freeze: ~/.emacs.d/straight/versions/default.el.
- `M-Q` unfill package.
- crux e.g. `H-o` for new line.
- doom-modeline
- minions
- pocket-reader.
- engine-mode.
- calibredb.
- company-box.
- org-ref v3 and ORB, org-roam v2 and org-protocol.
- pass
- org-lint

### Changed

- **PS1** multi line prompt.
- **brightnessctl** for light control.
- netctl for eduroam (hash:iconv -t utf16le | openssl md4).
- .bash .profile and .gitconfig.
- goldendict config.
- ~/.gnupg/gpg-agent.conf.
- ~/.ssh/config.
- ~/.config/gtk-3.0/ bookmarks and settings.ini.
- dunstrc.
- i3 and i3status (`Mod-F5` for zotero).
- ~/.mbsyncrc 1.4.1.
- (aliases) `xrr xr2 tj tji`.
- aurvote.
- ~/.xinitrc.
- ~/.inputrc.
- `C-x` fzf in ranger.
- rifle.conf and mimeview.
- recoll.conf.

#### emacs

- Default to biblatex dialect.
- Disable proselint in favor of AUR/vale.
- avy-goto `M-g c|w|l`.
- transpose-frame `H-e`.
- diff-hl instead of git-gutter.

### Removed

- (emacs) keychord.
- (emacs) elfeed.
- (emacs) eyebrowse.
- (emacs) slow mu4e maildir-extention.

### Fixed

- To work with emacs 27.1 (27.2).
- Slow response in e.g. .bib (wc-mode -1).

## [dropevil] 2.0.0 - 2020-04-19

### Added

- Define F14 `MenuKey` in Xmodmap for emacs modalka.
- goldendict HOME set in ~/.pam_environment.
- (emacs) Modalka.
- (emacs) mk-utils mk-text from mrkkrp packages.el.
- (emacs) hl-todo using a hydra `F14 2`.
- (emacs) Smart mode line.
- (emacs) Electric and aggressive indent.

### Changed

- CapsLock-Ctrl swapped
- (emacs) Use emacs keybindings instead of evil.
- (emacs) Several **which-key-add-key-based-replacements**.
- (emacs) .init.el flychecked and aggressively indented.

### Removed

- (emacs) evil, keychord and general.

## [vanilla] 1.0.0 - 2020-03-15

### Added

- This **Changelog.md** file.
- `povo()` YR forecast and `TNv()` in ~/.aliases.
- Configuration in init.el with use-package.
- Large use of evil and general packages to emulate vi.
- added textlint through npm.
- (emacs) vanilla configuration with evil keybinding.
- (emacs) a section for "emacs" in README.md.

### Changed

- location of the box folder moved to ~/Sync.
- `pip_updall` in ~/.aliases.
- bookmarks of gtk-3.0.
- Update revealjs as submodule in ~/.pandoc.
- psd.
- goldendict.
- ranger.
- .gitignore.
- (rifle) thunderbird.

### Removed

- spacemacs folders.
- (emacs) ~/.emacs.d/config/my-gtd.el.

## [Unreleased]

I leave history intact for all changes happened before I started using emacs.

### [spacemacs] 2019-09-16

- When I discovered emacs thanks to spacemacs.
