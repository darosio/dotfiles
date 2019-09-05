# Changelog
All notable changes to my dotfiles repo will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

Releases will start from 2022.1.

## [straight] - 2022-01-30
### Added
- Staight for much improved bootstrap. (Does package order now matter?)
- New file ~/.function for function command in bash like `rga-fzf` for rip-grep-all.
- smartd.conf in 4root.
- systemd/network/*.network files.
- wpa_supplicant for wired eduroam.
- gmail-isync
#### emacs
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

## [dropevil] - 2020-04-19
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

## [vanilla] - 2020-03-15
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
