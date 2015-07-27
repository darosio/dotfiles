# Repository for $HOME/.dotfiles

## Requirements

1. GNU stow 

## Usage

to create symlink into $HOME folder:

	cd ~/.dotfile
	stow package-version 
	
pick package-version as needed.

Plan to use machine-specific git branches.

I location of .dotfiles is moved please first:

stow -t ~/ misc
stoe -t ~/ bash
