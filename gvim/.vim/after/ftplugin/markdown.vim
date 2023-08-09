set modelines=1	" modeline must be within first or last line

" vim-pandoc {{{
let g:pandoc#filetypes#handled = ["pandoc", "markdown", "rst"]
let g:pandoc#filetypes#handled = ["markdown", "rst", "textile"] "default
" need bibtool; autocomplete using <C-X> <C-O>
let g:pandoc#biblio#use_bibtool = 0
let g:pandoc#folding#level = 2
let g:pandoc#folding#fdc = 4
" need this to make tagbar work
let g:pandoc#filetypes#pandoc_markdown = 1 "default
let g:pandoc#synatx#codeblock#embeds#langs = ['ruby', 'vim', 'python', 'r', 'json', 'c', 'julia', 'make', 'sh', 'latex']
" }}}
" vim-pandoc-after {{{
let g:pandoc#after#modules#enabled = ["supertab", "ultisnips"]
" }}}
setlocal guifont=Cousine\ 12
setlocal nonumber
nmap <F9> :TOC<CR>

" Dr Bunsen
"func! WordProcessorMode()
setlocal formatoptions=1
setlocal noexpandtab
map j gj
map k gk
set thesaurus+=/home/dan/.vim/thesaurus/mthesaur.txt
set formatprg=par
setlocal wrap
setlocal linebreak

" SPELL
"switch spellcheck languages
let g:myLang = 0
let g:myLangList = [ "nospell", "en_us" , "it"]
function! MySpellLang()
  "loop through languages
  let g:myLang = g:myLang + 1
  if g:myLang >= len(g:myLangList) | let g:myLang = 0 | endif
  if g:myLang == 0 | set nospell | endif
  if g:myLang == 1 | setlocal spell spelllang=it | endif
  if g:myLang == 2 | setlocal spell spelllang=en_us | endif
  echo "language:" g:myLangList[g:myLang]
endf
"zg		add word in ~.vim/spell
map <F6> :call MySpellLang()<CR>
imap <F6> <C-o>:call MySpellLang()<CR>

noremap Q gqap
" search and correct misspelled words ]s [s \s
nnoremap \s ea<C-X><C-S>
nnoremap \t ea<C-X><C-T>

" COMPLETE
" http://vim.wikia.com/wiki/Dictionary_completions
" @ponder https://github.com/reedes/vim-lexical
"set complete+=s "use thesaurus and was doing a lot of confusion"
" I would rather use default: .,w,b,u,],i
"set complete=.,b,u,]
set complete+=kspell

""" Abbreviation (Macros)
source ~/.vim/keystrokes.vim

" vim:foldmethod=marker:foldlevel=0
