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

setlocal expandtab tabstop=2 shiftwidth=2
setlocal textwidth=80
setlocal autoindent
setlocal formatoptions=tcroqnj1
setlocal comments=bf:-,bf:*,bf:@,bf:$,bf:o,bf:x,bf:+,bf:=,bf:>,bf:#,bf:::

function! s:indent()
  let hl = map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
  if empty(hl)
    return -1
  endif
  return max(add(map(filter(hl, "v:val =~ '^indent[0-9]*$'"),
                   \ "str2nr(substitute(v:val, '[^0-9]', '', 'g'))"), 0))
endfunction

function! s:progress(fw)
  normal! ^
  let oc = col('.')
  let oi = s:indent()
  let cond = a:fw ? "line('.') < line('$')" : "line('.') > 1"
  let dir = a:fw ? 'j^' : 'k^'
  while eval(cond)
    execute 'normal!' dir
    let c = col('.')
    let e = empty(getline('.'))
    let i = s:indent()
    if i >= 0 && !e && (i != oi || i == oi && c <= oc)
      break
    endif
  endwhile
  if i >= 0 && getline('.')[col('.') - 1:] =~ '^'.journal#_bullets()
    normal! w
  endif
endfunction

nnoremap <buffer><silent> [[ :call <sid>progress(0)<CR>
nnoremap <buffer><silent> ]] :call <sid>progress(1)<CR>
xnoremap <buffer><silent> [[ <ESC>:execute 'normal! gv'<BAR>call <sid>progress(0)<CR>
xnoremap <buffer><silent> ]] <ESC>:execute 'normal! gv'<BAR>call <sid>progress(1)<CR>

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

