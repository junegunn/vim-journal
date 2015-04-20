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

let s:max_indent = get(g:, 'journal#max_indent', 10)
let s:color_index = get(g:, 'journal#color_index', 0)

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

function! s:compare_h(c1, c2)
  let h1 = s:rgbhsl(a:c1)[0]
  let h2 = s:rgbhsl(a:c2)[0]
  return h1 == h2 ? 0 : h1 > h2 ? 1 : -1
endfunction

function! s:extract_colors()
  let blacklist = s:blacklist()
  let defnormal = &background == 'dark' ? 253 : 233
  let [h0, s0, l0] = s:rgbhsl(empty(blacklist) ? defnormal : keys(blacklist)[0])
  for c in get(g:, 'journal#blacklist', [])
    let blacklist[c] = 1
  endfor

  redir => output
    silent hi
  redir END
  let colors = {}
  for line in filter(split(output, '\n'), 'v:val =~# "fg" && v:val !~# "bg"')
    let fg = s:extract_fg(line)
    if empty(fg)
      continue
    endif

    let [h, s, l] = s:rgbhsl(fg)
    if !has_key(blacklist, fg) && abs(l - l0) < 0.4 && s < 0.6
      let colors[fg] = 1
    endif
  endfor
  let list = keys(colors)
  if len(colors) > s:max_indent
    let trimmed = []
    let ratio = 1.0 * len(list) / s:max_indent
    let idx = 0.0
    while len(trimmed) < s:max_indent
      call add(trimmed, list[float2nr(idx)])
      let idx += ratio
    endwhile
    let list = trimmed
  endif

  return sort(list, function('s:compare_h'))
endfunction

" http://stackoverflow.com/questions/27159322/rgb-values-of-the-colors-in-the-ansi-extended-colors-index-17-255
let s:ansi16 = {
  \ 0:  '#000000', 1:  '#800000', 2:  '#008000', 3:  '#808000',
  \ 4:  '#000080', 5:  '#800080', 6:  '#008080', 7:  '#c0c0c0',
  \ 8:  '#808080', 9:  '#ff0000', 10: '#00ff00', 11: '#ffff00',
  \ 12: '#0000ff', 13: '#ff00ff', 14: '#00ffff', 15: '#ffffff' }
function! s:rgb(color)
  if a:color[0] == '#'
    let r = str2nr(a:color[1:2], 16)
    let g = str2nr(a:color[3:4], 16)
    let b = str2nr(a:color[5:6], 16)
    return [r, g, b]
  endif

  let ansi = str2nr(a:color)

  if ansi < 16
    return s:rgb(s:ansi16[ansi])
  endif

  if ansi >= 232
    let v = (ansi - 232) * 10 + 8
    return [v, v, v]
  endif

  let r = (ansi - 16) / 36
  let g = ((ansi - 16) % 36) / 6
  let b = (ansi - 16) % 6

  return map([r, g, b], 'v:val > 0 ? (55 + v:val * 40) : 0')
endfunction

" http://stackoverflow.com/questions/2353211/hsl-to-rgb-color-conversion
function! s:hsl(rgb)
  let [max, min] = map([max(a:rgb), min(a:rgb)], 'v:val / 255.0')
  let [r, g, b]  = map(a:rgb, 'v:val / 255.0')
  let h = (max + min) / 2.0
  let s = h
  let l = h

  if max == min
    return [0, 0, h]
  endif

  let d = max - min
  let s = l > 0.5 ? d / (2 - max - min) : d / (max + min)

  if     max == r | let h = (g - b) / d + (g < b ? 6 : 0)
  elseif max == g | let h = (b - r) / d + 2
  elseif max == b | let h = (r - g) / d + 4
  endif
  let h = h / 6.0
  return [h, s, l]
endfunction

let s:rgbhsl = {}
function! s:rgbhsl(color)
  if has_key(s:rgbhsl, a:color)
    return s:rgbhsl[a:color]
  endif
  let hsl = s:hsl(s:rgb(a:color))
  let s:rgbhsl[a:color] = hsl
  return hsl
endfunction

syn match indentBullet0 /^[-@#$*:xo0-9+>=][.:)]\?\s/
hi def link indentBullet0 Label

syn region topLevel start="^[0-9A-Z].*" end="$" contains=ALL
hi def link topLevel Directory

syn match url %https\?://\(\w\+\(:\w\+\)\?@\)\?[A-Za-z0-9-_.]*\(:[0-9]\{1,5}\)\?\S*%
hi def link url Underlined

syn keyword bool true false
hi def link bool Boolean

syn keyword weekday MON TUE WED THU FRI Mon Tue Wed Thu Fri mon tue wed thu fri
syn keyword weekend SAT SUN Sat Sun sat sun
hi def link weekday WarningMsg
hi def link weekend MoreMsg

syn keyword month JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
syn keyword month Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
syn keyword month jan feb mar apr may jun jul aug sep oct nov dec
hi def link month Constant

syn region blockComment start="/\*" end="\*/"
hi def link blockComment Comment

syn region codeSpan start="`" end="`"
hi def link codeSpan String

syn region strong oneline matchgroup=strongSign start="\*\ze\S" end="\S\zs\*"
hi def link strong Question
hi def link strongSign Exception

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
  for i in range(1, s:max_indent)
    let indent = i * &tabstop
    let allbut = i == 1 ? 'ALL' : 'ALLBUT,'.join(map(range(1, i), '"indent".v:val'), ',')
    execute printf('syn region indent%d start=/^\s\{%d,}[-@#$*:xo0-9+>=][.:)]\?\s/ end=/$/ contains=%s', i, indent, allbut)
    execute printf('syn region indent%d start=/^\s\{%d,}  \([-@#$*:xo0-9+>=]\s\)\@<!/ end=/$/ contains=%s', i, indent, allbut)
    execute printf('syn match indentBullet%d  /^\s\{%d,}[-@#$*:xo0-9+>=][.:)]\?\s/ containedin=indent%d contained', i, indent, i)
    execute printf('syn match indentValue /\S:\s\+\zs.\{}/ containedin=indent%d contained', i)
    if !empty(colors)
      let cidx = i - 1 + s:color_index
      let col  = colors[cidx % len(colors)]
      let bcol = colors[(cidx + s:max_indent / 2) % len(colors)]
      execute printf('hi indent%d %sfg=%s',       i, has('gui') ? 'gui' : 'cterm', col)
      execute printf('hi indentBullet%d %sfg=%s', i, has('gui') ? 'gui' : 'cterm', bcol)
    endif
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

