" Copyright (c) 2015 Junegunn Choi
"
" MIT License
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

setlocal textwidth=78
setlocal autoindent
setlocal formatoptions=tcroqn1
silent! setlocal formatoptions+=j
setlocal comments=bf:-,bf:*,bf:@,bf:$,bf:o,bf:x,bf:+,bf:=,bf:>,bf:#,bf:::
" setlocal synmaxcol=160

function! s:hl()
  return map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

function! s:indent()
  let hl = s:hl()
  if empty(hl)
    return -1
  endif
  return max(add(map(filter(hl, "v:val =~ '^indent[0-9]*$'"),
                   \ "str2nr(substitute(v:val, '[^0-9]', '', 'g'))"), 0))
endfunction

function! s:progress(fw, cnt)
  let cnt = a:cnt
  while cnt
    let opos = getpos('.')
    while 1
      let pos = getpos('.')
      call search('^\k', a:fw ? '' : 'b')
      if get(s:hl(), 0, '') == 'topLevel'
        let cnt -= 1
        break
      endif
      if getpos('.') == pos
        call setpos('.', opos)
        return
      endif
    endwhile
  endwhile
endfunction

" <C-U> is required to correctly handle counts
nnoremap <buffer> <silent> [[ :<c-u>call <sid>progress(0, v:count1)<CR>
nnoremap <buffer> <silent> ]] :<c-u>call <sid>progress(1, v:count1)<CR>
xnoremap <buffer> <silent> [[ :<c-u>call <sid>progress(0, v:count1)<CR>``gv``
xnoremap <buffer> <silent> ]] :<c-u>call <sid>progress(1, v:count1)<CR>``gv``

nnoremap <buffer> ][ <nop>
nnoremap <buffer> [] <nop>
xnoremap <buffer> ][ <nop>
xnoremap <buffer> [] <nop>

function! s:bullet()
  let line = getline('.')
  let indent = matchstr(line, '^\s*')
  let rest = line[len(indent):]
  let bullet = matchstr(rest, '^'.journal#_bullets().'\+')
  if empty(bullet)
    return "\<cr>"
  elseif bullet =~ '^[0-9]'
    let match = matchlist(bullet, '^\([0-9]\+\)\(.\)')
    let num   = str2nr(match[1])
    let tail  = match[2]
    return "\<cr>".(num + 1).tail." "
  elseif bullet =~ '^\[.\]'
    return "\<cr>".bullet
  endif
  let ret = "\<cr>"
  for _ in range(len(bullet) / &shiftwidth)
    let ret .= "\<bs>"
  endfor
  return ret.bullet
endfunction

inoremap <buffer> <expr> <esc><cr> <sid>bullet()
nnoremap <buffer> <expr> <esc><cr> 'A'.<sid>bullet()

