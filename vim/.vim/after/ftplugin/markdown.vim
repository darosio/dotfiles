set modelines=1	" modeline must be within first or last 1 line

" vim-wordy {{{
let g:wordy#ring = [
  \ 'weak',
  \ 'weasel',
  \ 'puffery',
  \ 'redundant',
  \ 'problematic',
  \ ['being', 'passive-voice', ],
  \ ['business-jargon', 'art-jargon',],
  \ ['colloquial', 'idiomatic', ],
  \ 'similies',
  \ 'said-synonyms',
  \ 'contractions',
  \ 'opinion',
  \ 'vague-time',
  \ ]
" to cycle among defined rings
nnoremap <silent> K :NextWordy<cr>
" }}}
" languagetool {{{
let g:languagetool_jar='/usr/share/java/languagetool/languagetool-commandline.jar'
"let g:DirDiffDynamicDiffText = 1
"}}}
" vim-pandoc {{{
let g:pandoc#filetypes#handled = ["pandoc", "markdown", "rst"]
let g:pandoc#filetypes#handled = ["markdown", "rst", "textile"] "default
" need bibtool; autocomplete using <C-X> <C-O>
let g:pandoc#biblio#use_bibtool = 1
"let g:pandoc#biblio#bibs = ['~/Sync/00-papers.bib']

let g:pandoc#completion#bib#mode = "citeproc"
" fallback much faster, but not working in python3, consider unite-bibtex
"let g:pandoc#formatting#mode = "A"
" avoid spell check of citations like @Arosio2007
"""syntax match myExNonWords +\<\p*[^A-Za-z \t]\p*\>+ contains=@NoSpell
let g:pandoc#folding#level = 2
let g:pandoc#folding#fdc = 4
" need this to make tagbar work
let g:pandoc#filetypes#pandoc_markdown = 1 "default
let g:pandoc#synatx#codeblock#embeds#langs = ['ruby', 'vim', 'python', 'r', 'json', 'c', 'julia', 'make', 'sh', 'latex']
" }}}
" vim-pandoc-after {{{
let g:pandoc#after#modules#enabled = ["supertab", "ultisnips", "nrrwrgn"]
" }}}
" not needed in i3
" setlocal lines=38
" setlocal columns=136
"setlocal background=light
setlocal guifont=Cousine\ 12
setlocal nonumber

nmap <F9> :TOC<CR>

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
map <F6> :call MySpellLang()<CR>
imap <F6> <C-o>:call MySpellLang()<CR>
nmap <F7> :GrammarousCheck<CR>
nmap <F8> <Plug>grammarous-open-info-window<CR>


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
" I would rather use default: .,w,b,u,],i
"set complete=.,b,u,]
set complete+=kspell 

""" Abbreviation (Macros)
source ~/.vim/keystrokes.vim

" vim:foldmethod=marker:foldlevel=0
