" not needed in i3
" setlocal lines=38
" setlocal columns=136
"setlocal background=light
setlocal guifont=Cousine\ 12

" Dr Bunsen
"func! WordProcessorMode() 
setlocal formatoptions=1
setlocal noexpandtab 
map j gj 
map k gk
" setlocal spell spelllang=en_us,it 
set thesaurus+=/home/dan/.vim/thesaurus/mthesaur.txt
set formatprg=par
setlocal wrap 
setlocal linebreak
"endfu 
"com! WP call WordProcessorMode()

"switch spellcheck languages
let g:myLang = 0
let g:myLangList = [ "nospell", "it_it", "en_us" ]
function! MySpellLang()
  "loop through languages
  let g:myLang = g:myLang + 1
  if g:myLang >= len(g:myLangList) | let g:myLang = 0 | endif
  if g:myLang == 0 | set nospell | endif
  if g:myLang == 1 | setlocal spell spelllang=it_it | endif
  if g:myLang == 2 | setlocal spell spelllang=en_us | endif
  echo "language:" g:myLangList[g:myLang]
endf
map <F7> :call MySpellLang()<CR>
imap <F7> <C-o>:call MySpellLang()<CR>

"setlocal formatoptions=t1 
" reverse with =croql
"setlocal textwidth=78 
" autoformat paragraph in insert mode: I was annoyed 
"augroup PROSE
"    autocmd InsertEnter * set formatoptions+=a
"    autocmd InsertLeave * set formatoptions-=a
"augroup END
noremap Q gqap
" search and correct mispelled words ]s [s \s
nnoremap \s ea<C-X><C-S>
nnoremap \t ea<C-X><C-T>

"zg		add word in ~.vim/spell
set complete+=s
set complete=.,b,u,]
set complete+=kspell 

" vim-pandoc {{{
" need bibtool; autocomplete using <C-X> <C-O>
let g:pandoc#biblio#use_bibtool=1
let g:pandoc#completion#bib#mode = 'citeproc'
" avoid spell check of citations like @Arosio2007
syntax match myExNonWords +\<\p*[^A-Za-z \t]\p*\>+ contains=@NoSpell
" }}}

""" Abbreviation (Macros)
source ~/.vim/keystrokes.vim


" vim:foldmethod=marker:foldlevel=0
