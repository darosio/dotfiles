#!/bin/sh
emacs --eval "(ediff (file-truename \"$1\") (file-truename \"$2\"))"
# emacsclient -c -a="" --eval "(let ((ediff-split-window-function 'split-window-horizontally)) (ediff (file-truename \"$1\") (file-truename \"$2\")))"
