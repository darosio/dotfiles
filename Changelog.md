# Changelog
All notable changes to my dotfiles repo will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

Releases will start from 2022.1.


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
