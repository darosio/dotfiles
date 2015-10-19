set modelines=1	" modeline must be within first or last 1 line

set tabstop=8
set expandtab
set shiftwidth=4
set softtabstop=4
set textwidth=79
set autoindent

" jedi {{{
let g:jedi#auto_initialization = 1
let g:jedi#auto_vim_configuration = 1
let g:jedi#popup_on_dot = 0         " To disable auto-complete on .
let g:jedi#use_splits_not_buffers = "right"
let g:jedi#popup_select_first = 1
let g:jedi#show_call_signatures = 1
"setlocal completeopt-=preview "prevent docstring split window
"setlocal omnifunc=python3complete#Complete
setlocal omnifunc=jedi#completions
" }}}
" Pytest {{{
nmap <silent><Leader>rf <Esc>:Pytest file<CR>
nmap <silent><Leader>rc <Esc>:Pytest class<CR>
nmap <silent><Leader>rm <Esc>:Pytest method<CR>
" }}}
" virtualenv {{{
" FIXME not needed?
let g:virtualenv_directory = '/home/dan/.venvs'
let g:virtualenv_auto_activate = 1
let g:virtualenv_stl_format = '[%n]'
"%{virtualenv#statusline()}
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
" }}}

" use Screen.vim for sending cmd to ipython
vmap <CR> :ScreenSend<CR><esc>'>j "multiline
nmap <CR> V :ScreenSend<CR>ej "multiline


" vim:foldmethod=marker:foldlevel=0
