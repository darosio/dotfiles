set tabstop=8
set expandtab
set shiftwidth=4
set softtabstop=4

" Add the virtualenv's site-packages to vim path
" works only for python2 and iseems not needed to pick up libraries
" py py3 << EOF
" import os.path
" import sys
" import vim
" if 'VIRTUAL_ENV' in os.environ:
"     project_base_dir = os.environ['VIRTUAL_ENV']
"     sys.path.insert(0, project_base_dir)
"     activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
"     execfile(activate_this, dict(__file__=activate_this))
" EOF
