" Usage:
"   Copy this file to your ~/.vim/plugin/ directory.
"       $ vim -O oldfile newfile
"   OR
"       $ vim -d oldfile newfile
"   :Wd

let s:save_cpo = &cpo
set cpo&vim

function Wdiff()
  let Afile = expand('%')
  wincmd w
  let Bfile = expand('%')
  wincmd p

  let [Amatches, Bmatches] = s:wdiff(Afile, Bfile)
  call s:highlight(bufwinnr(Afile), Amatches)
  call s:highlight(bufwinnr(Bfile), Bmatches)
  wincmd p
endfunction

function s:highlight(winnr, matches)
  let fg = synIDattr(synIDtrans(hlID("DiffText")), "fg")
  let bg = synIDattr(synIDtrans(hlID("DiffText")), "bg")
  if has("gui_running")
    let sp = synIDattr(synIDtrans(hlID("DiffText")), "sp")
    let bold = synIDattr(synIDtrans(hlID("DiffText")), "bold")
    execute "highlight default DiffWord" (bold ? "gui=bold" : "") "guifg=". fg "guibg=". bg "guisp=". sp
  else
    let reverse = synIDattr(synIDtrans(hlID("DiffText")), "reverse")
    execute "highlight default DiffWord" (reverse ? "term=reverse cterm=reverse" : "") "ctermfg=". fg "ctermbg=". bg
  endif
  " Vim gets this wrong, make it invisible and use our DiffWord instead
  highlight! link DiffText DiffChange
  " With -d, leave only the affected words highlighted, not the whole line
  "highlight! clear DiffChange
  execute a:winnr . 'wincmd w'
  setlocal scrollbind
  setlocal cursorbind
  setlocal nowrap
  setlocal foldmethod=diff
  setlocal foldenable
  call clearmatches()
  for lnum in keys(a:matches)
    for col in keys(a:matches[lnum])
      let word = a:matches[lnum][col][0]
      let higroup = a:matches[lnum][col][1]
      let pat = printf('\V\%%%dl\%%%dc%s', lnum + 1, col + 1, escape(word, '\'))
      call matchadd(higroup, pat)
    endfor
  endfor
endfunction

function s:wdiff(Afile, Bfile)
  let output = system("wdiff ". a:Afile ." ". a:Bfile)
  let lines = split(output, "\n")
  let Amatches = {}
  let Bmatches = {}
  let old = '\[-\([^]]\{-1,\}\)-\]'
  let new = '{+\([^}]\{-1,\}\)+}'
  let changed = old .' '. new
  let any = old .'\|'. new
  let deleted = '\[-\([^]]\{-1,\}\)-\]\( {+\)\@!'
  let inserted = '\(-] \)\@<!{+\([^}]\{-1,\}\)+}'
  let l = 0
  let Al = 0
  let Bl = 0
  while l != -1
    let l = match(lines, any, l)
    if l == -1
      continue
    endif
    let c = 0
    let line = lines[l]
    let c = match(line, changed, c)
    if c != -1
      let Aline = substitute(line, deleted, "\\1", "g")
      let Aline = substitute(Aline, ' '. inserted, "", "g")
      let Aline = substitute(Aline, ' '. new, "", "g")
      let c = 0
      let surround = 0
      while c != -1
        let c = match(Aline, deleted, c)
        if c != -1
          let Amatch = matchstr(Aline, deleted, c)
          call extend(Amatches, {l + Al : {}}, "keep")
          call extend(Amatches[l + Al], {(c - surround): [substitute(Amatch, deleted, "\\1", ""), "DiffWord"]}, "keep")
          let surround += 4
          let c += len(Amatch)
        endif
      endwhile
      let Bline = substitute(line, inserted, "\\2", "g")
      let Bline = substitute(Bline, ' '. deleted, "", "g")
      let Bline = substitute(Bline, ' '. old, "", "g")
      let c = 0
      let surround = 0
      while c != -1
        let c = match(Bline, inserted, c)
        if c != -1
          let Bmatch = matchstr(Bline, inserted, c)
          call extend(Bmatches, {l + Bl : {}}, "keep")
          call extend(Bmatches[l + Bl], {(c - surround): [substitute(Bmatch, inserted, "\\2", ""), "DiffWord"]}, "keep")
          let surround += 4
          let c += len(Bmatch)
        endif
      endwhile
    endif
    let line = substitute(lines[l], ' '. new, "", "g")
    let c = 0
    let surround = 0
    while c != -1
      let c = match(line, deleted, c)
      if c != -1
        let Amatch = matchstr(line, deleted, c)
        let Bl -= (len(line) == len(Amatch))
        call extend(Amatches, {l + Al : {}}, "keep")
        call extend(Amatches[l + Al], {(c - surround): [substitute(Amatch, deleted, "\\1", ""), "DiffDelete"]}, "keep")
        let surround += 4
        let c += len(Amatch)
      endif
    endwhile
    let line = substitute(lines[l], ' '. old, "", "g")
    let c = 0
    let surround = 0
    while c != -1
      let c = match(line, inserted, c)
      if c != -1
        let Bmatch = matchstr(line, inserted, c)
        let Al -= (len(line) == len(Bmatch))
        call extend(Bmatches, {l + Bl : {}}, "keep")
        call extend(Bmatches[l + Bl], {(c - surround): [substitute(Bmatch, inserted, "\\2", ""), "DiffAdd"]}, "keep")
        let surround += 4
        let c += len(Bmatch)
      endif
    endwhile
    let l += 1
  endwhile
  return [Amatches, Bmatches]
endfunction

command Wdiff call Wdiff()

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:shiftwidth=2:expandtab
