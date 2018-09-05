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

## submodule problem
I had once and solved the detached head problem following:
https://stackoverflow.com/questions/18770545/why-is-my-git-submodule-head-detached-from-master

git branch -u refs/remotes/origin/master master

check .gitmodule
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
