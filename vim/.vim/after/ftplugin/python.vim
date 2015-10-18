set modelines=1	" modeline must be within first or last 1 line

" jedi {{{
" FIXME move these into ./after/ftplugin/python.vim
" forse supertab non serve basta ctrl-space
" To disable auto-complete on .
let g:jedi#popup_on_dot = 0
"let g:jedi#use_splits_not_buffers = "bottom"
" }}}
" Pytest {{{
nmap <silent><Leader>rf <Esc>:Pytest file<CR>
nmap <silent><Leader>rc <Esc>:Pytest class<CR>
nmap <silent><Leader>rm <Esc>:Pytest method<CR>
" }}}
" virtualenv {{{
let g:virtualenv_directory = '/home/dan/workspace/venvs'
let g:virtualenv_auto_activate = 1
let g:virtualenv_stl_format = '[%n]'
"%{virtualenv#statusline()}
" }}}
" use Screen.vim for sending cmd to ipython
vmap <CR> :ScreenSend<CR> <esc> "multiline
nmap <CR> V :ScreenSend<CR>ej "multiline
" configure jedi from my old CHECK TODO
let g:jedi#auto_initialization = 0
let g:jedi#pydoc = "K"
" keybind: pydoc
nnoremap <buffer> K <ESC>:call jedi#show_pydoc()<CR>
"setlocal omnifunc=python3complete#Complete
setlocal omnifunc=jedi#completions

set tabstop=8
set expandtab
set shiftwidth=4
set softtabstop=4
set textwidth=79
set autoindent

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

" vim:foldmethod=marker:foldlevel=0
