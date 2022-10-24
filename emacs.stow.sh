#!/usr/bin/bash

mkdir -p $HOME/.emacs.d/straight/versions
stow -t $HOME emacs
cd $HOME/.emacs.d
ln -s $HOME/Sync/.emacs/* .
