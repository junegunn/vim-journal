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

if exists("b:current_syntax")
  finish
endif

syn clear

function! s:blacklist()
  redir => output
    silent hi Normal
  redir END
  let line = split(output, '\n')[0]
  let fg   = s:extract_fg(line)
  let blacklist = {}
  if !empty(fg) | let blacklist[fg] = 1 | endif
  return blacklist
endfunction

function! s:extract_fg(line)
  return matchstr(a:line, (has('gui') ? 'gui' : 'cterm').'fg=\zs\S*\ze')
endfunction

function! s:extract_colors()
  let blacklist = s:blacklist()
  for c in get(g:, 'journal#blacklist', [])
    let blacklist[c] = 1
  endfor

  redir => output
    silent hi
  redir END
  let colors = {}
  for line in filter(split(output, '\n'), 'v:val =~# "fg" && v:val !~# "bg"')
    let fg =s:extract_fg(line)
    if !empty(fg) && !has_key(blacklist, fg)
      let colors[fg] = 1
    endif
  endfor
  return keys(colors)
endfunction

syn match indentBullet0 /^[-@#$*:xo0-9+>=][.:)]\?\s/
hi def link indentBullet0 Label

syn region topLevel start="^[0-9A-Z].*" end="$" contains=ALL
hi def link topLevel Directory

syn match url %https\?://\(\w\+\(:\w\+\)\?@\)\?[A-Za-z0-9-_.]*\(:[0-9]\{1,5}\)\?\S*%
hi def link url Underlined

syn keyword bool true false
hi def link bool Boolean

syn keyword weekday MON TUE WED THU FRI
syn keyword weekend SAT SUN
hi def link weekday WarningMsg
hi def link weekend MoreMsg

syn region blockComment start="/\*" end="\*/"
hi def link blockComment Comment

syn region codeSpan start="`" end="`"
hi def link codeSpan String

syn match reference /\[[0-9]\+\]/
hi def link reference Keyword

syn match separator /^[-=]\+$/
hi def link separator Structure

function! s:syntax_include(lang, b, e, inclusive)
  let syns = split(globpath(&rtp, "syntax/".a:lang.".vim"), "\n")
  if empty(syns)
    return
  endif

  if exists('b:current_syntax')
    let csyn = b:current_syntax
    unlet b:current_syntax
  endif

  let z = "'" " Default
  for nr in range(char2nr('a'), char2nr('z'))
    let char = nr2char(nr)
    if a:b !~ char && a:e !~ char
      let z = char
      break
    endif
  endfor

  silent! exec printf("syntax include @%s %s", a:lang, syns[0])
  if a:inclusive
    exec printf('syntax region %sSnip start=%s\(\)\(%s\)\@=%s ' .
                \ 'end=%s\(%s\)\@<=\(\)%s contains=@%s containedin=ALL',
                \ a:lang, z, a:b, z, z, a:e, z, a:lang)
  else
    exec printf('syntax region %sSnip matchgroup=Snip start=%s%s%s ' .
                \ 'end=%s%s%s contains=@%s containedin=ALL',
                \ a:lang, z, a:b, z, z, a:e, z, a:lang)
  endif

  if exists('csyn')
    let b:current_syntax = csyn
  endif
endfunction

function! s:init()
  let colors = s:extract_colors()
  for i in range(1, 10)
    let indent = i * &tabstop
    let allbut = i == 1 ? 'ALL' : 'ALLBUT,'.join(map(range(1, i), '"indent".v:val'), ',')
    execute printf('syn region indent%d start=/^\s\{%d,}[-@#$*:xo0-9+>=][.:)]\?\s/ end=/$/ contains=%s', i, indent, allbut)
    execute printf('syn region indent%d start=/^\s\{%d,}  [^-@#$*:xo0-9+>=]/ end=/$/ contains=%s', i, indent, allbut)
    execute printf('syn match indentBullet%d  /^\s\{%d,}[-@#$*:xo0-9+>=][.:)]\?\s/ containedin=indent%d contained', i, indent, i)
    execute printf('syn match indentValue /\S:\s\+\zs.\{}/ containedin=indent%d contained', i)
    execute printf('hi indent%d %sfg=%s', i, has('gui') ? 'gui' : 'cterm', colors[i % len(colors)])
    execute printf('hi indentBullet%d %sfg=%s', i, has('gui') ? 'gui' : 'cterm', colors[(i + 3) % len(colors)])
    execute printf('hi def link indentValue%d Normal', i)
  endfor

  " TODO
  for lang in ['ruby', 'yaml', 'vim', 'sh', 'python', 'java', 'c', 'sql', 'clojure']
    call s:syntax_include(lang, '```'.lang, '```', 0)
  endfor
  hi def link Snip Folded
endfunction

augroup journal
  autocmd!
  autocmd ColorScheme * call s:init()
augroup END
call s:init()

let b:current_syntax = 'journal'

