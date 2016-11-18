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

function! s:colfam()
  return (has('gui_running') || has('termguicolors') && &termguicolors) ? 'gui' : 'cterm'
endfunction

function! s:extract_fg(line)
  return matchstr(a:line, s:colfam().'fg=\zs\S*\ze')
endfunction

function! s:compare_h(c1, c2)
  let h1 = s:rgbhsl(a:c1).h
  let h2 = s:rgbhsl(a:c2).h
  return h1 == h2 ? 0 : h1 > h2 ? 1 : -1
endfunction

let s:default_color_filter = [
  \ function('journal#color#filter#readable'),
  \ function('journal#color#filter#low_saturation')]

function! s:extract_colors(max_count)
  let blacklist = s:blacklist()
  let defnormal = &background == 'dark' ? 253 : 233
  let normhsl = s:rgbhsl(empty(blacklist) ? defnormal : keys(blacklist)[0])
  for c in get(g:, 'journal#blacklist', [])
    let blacklist[c] = 1
  endfor

  redir => output
    silent hi
  redir END
  let all_colors = {}
  for line in filter(split(output, '\n'), 'v:val =~# "fg" && v:val !~# "bg"')
    let fg = s:extract_fg(line)
    if empty(fg)
      continue
    endif
    let all_colors[fg] = 1
  endfor

  let colors = keys(all_colors)
  for Fn in copy(get(g:, 'journal#color_filters', s:default_color_filter))
    let filtered = []
    for fg in colors
      let hsl = s:rgbhsl(fg)
      if !has_key(blacklist, fg) && Fn(hsl, normhsl)
        call add(filtered, fg)
      endif
    endfor
    if len(filtered) < a:max_count / 2
      break
    endif
    let colors = filtered
  endfor

  if len(colors) > a:max_count
    let trimmed = []
    let ratio = 1.0 * len(colors) / a:max_count
    let idx = 0.0
    while len(trimmed) < a:max_count
      call add(trimmed, colors[float2nr(idx)])
      let idx += ratio
    endwhile
    let colors = trimmed
  endif

  return sort(colors, function('s:compare_h'))
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
    return {'h': 0, 's': 0, 'l': h, 'p': h}
  endif

  let d = max - min
  let s = l > 0.5 ? d / (2 - max - min) : d / (max + min)

  if     max == r | let h = (g - b) / d + (g < b ? 6 : 0)
  elseif max == g | let h = (b - r) / d + 2
  elseif max == b | let h = (r - g) / d + 4
  endif
  let h = h / 6.0

  " http://stackoverflow.com/questions/596216/formula-to-determine-brightness-of-rgb-color
  " http://alienryderflex.com/hsp.html
  let p = sqrt(0.299 * r * r + 0.587 * g * g + 0.114 * b * b)
  return {'h': h, 's': s, 'l': l, 'p': p}
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

let s:bullets = journal#_bullets()
execute 'syn region indent0 start=/^'.s:bullets.'/ end=/^\ze\S/ contains=ALL fold'
execute 'syn match  indentBullet0 /^'.s:bullets.'/ containedin=indent0 contained'
hi def link indentBullet0 Label

syn match checkboxChecked   /\[\zs[xov]\ze\]/
syn match checkboxException /\[\zs[*!]\ze\]/
syn match checkboxEtc       /\[\zs[+=-]\ze\]/
syn cluster checkbox contains=checkboxChecked,checkboxException,checkboxEtc
hi def link checkboxChecked   Boolean
hi def link checkboxException Exception
hi def link checkboxEtc       Conditional

syn match url %https\?://\(\w\+\(:\w\+\)\?@\)\?[A-Za-z0-9-_.]*\(:[0-9]\{1,5}\)\?\S*%
hi def link url Underlined

syn keyword bool true false
hi def link bool Boolean

syn keyword weekday MON TUE WED THU FRI Mon Tue Wed Thu Fri
syn keyword weekend SAT SUN Sat Sun
hi def link weekday WarningMsg
hi def link weekend MoreMsg

syn keyword month JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
syn keyword month Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
hi def link month Constant

syn region logLevelIgnore start="\<\%(DEBUG\|TRACE\)" end="\>"
hi def link logLevelIgnore Comment
syn region logLevelInfo start="\<INFO" end="\>"
hi def link logLevelInfo Conditional
syn region logLevelWarn start="\<WARN" end="\>"
hi def link logLevelWarn WarningMsg
syn region logLevelError start="\<\%(ERROR\|FATAL\)" end="\>"
hi def link logLevelError Exception
syn region keywords start="\<\%(FIXME\|TODO\|XXX\)" end="\>"
hi def link keywords TODO

syn match date @\<[0-9]\{4}[-/][0-9]\{2}[-/][0-9]\{2}@
hi def link date Directory
syn match time @\<[0-9]\{2}:[0-9]\{2}:[0-9]\{2}\([,.][0-9]\+\)\?@
hi def link time Number

syn region codeSpan oneline start="`" end="`"
hi def link codeSpan String

syn region blockCode matchgroup=snippetDelimiter start="^\s*\zs```\S*$" end="^\s*\zs```$"
syn region blockCode matchgroup=snippetDelimiter start="^\s*\zs--\+[^[:blank:]-]*-*$" end="^\s*\zs--\+$"
hi def link blockCode String

syn region strong oneline matchgroup=strongSign start="\*\ze[^[:blank:]]" end="[^[:blank:]]\zs\*" contains=@Spell
hi def link strong Question
hi def link strongSign Exception

syn region intense oneline matchgroup=intenseSign start="\*\*\ze[^[:blank:]*]" end="[^[:blank:]*]\zs\*\*" contains=@Spell
hi def link intense Question
hi def link intenseSign Exception

syn region underline matchgroup=underlineSign start="\(^\|\s\)\zs_\ze\S" end="\S\zs_\ze\(\s\|$\|\W\)" contains=@Spell
hi def link underline Underlined
hi def link underlineSign NonText

syn match file @\(^\|\s\)\zs\(file://\)\?[/~].\{-}\ze\(:\|\\\@<!\s\|$\)@
hi def link file PreProc

syn region blockComment start="\(^\|\s\)\zs/\*" end="\*/"
hi def link blockComment Comment

syn match strongEnd /\*\*$/
syn match strongEnd1 /\*$/
syn match strongEnd2 /\*$/ containedin=strongEnd contained
hi def link strongEnd Exception
hi def link strongEnd1 Exception
hi def link strongEnd2 Question

syn region strikeThrough oneline matchgroup=strikeThroughSign start="\~\ze\S" end="\S\zs\~"
hi def link strikeThrough Ignore
hi def link strikeThroughSign NonText

syn match strikeThroughEnd /\~\~$/
syn match strikeThroughEnd1 /\~$/
syn match strikeThroughEnd2 /\~$/ containedin=strikeThroughEnd contained
hi def link strikeThroughEnd NonText
hi def link strikeThroughEnd1 NonText
hi def link strikeThroughEnd2 Ignore

syn match reference /\[[0-9]\{1,3}\]/ containedin=topLevel
hi def link reference Keyword

syn match topLevel /\%1l\%1c[^[:punct:] ].*$/ contains=ALLBUT,@NoSpell,topLevel
syn region topLevel start=/\%(^\s*$\n^\)\@<=[^[:punct:] ]/ end=/[^.,]\ze\n\%(\s*$\|[[:punct:] ]\)/ oneline contains=ALLBUT,@NoSpell,topLevel
hi def link topLevel Directory

syn match topLevel /^\S\+.*\n[-=]\+$/ contains=@Spell,topLevelUnderline
syn match topLevelUnderline /^[-=]\+$/

" syn match separator /^[-=]\+$/
hi def link topLevelUnderline Structure

" Code snippets
hi def link snippetDelimiter Folded

function! s:detect(l1, l2)
  let b:journal_types = get(b:, 'journal_types', {})
  let types = filter(map(getline(a:l1, a:l2), 'matchstr(v:val, ''^\s*\%(```\|--\+\)\zs\S*$'')'), '!empty(v:val) && !has_key(b:journal_types, v:val)')

  for lang in types
    call s:load(lang)
  endfor
  return types
endfunction

function! s:load(lang)
  let b:journal_types[a:lang] = 1
  let syns = split(globpath(&rtp, "syntax/".a:lang.".vim"), "\n")
  if empty(syns)
    return
  endif

  if exists('b:current_syntax')
    let csyn = b:current_syntax
    unlet b:current_syntax
  endif

  silent! execute printf("syntax include @%s %s", a:lang, syns[0])

  for [b, e] in [['^\s*\zs```'.a:lang.'$', '^\s*\zs```$'], ['^\s*\zs--\+'.a:lang.'-*$', '^\s*\zs--\+$']]

    let z = "'" " Default
    for nr in range(char2nr('a'), char2nr('z'))
      let char = nr2char(nr)
      if b !~ char && e !~ char
        let z = char
        break
      endif
    endfor

    exec printf('syntax region %sSnip matchgroup=snippetDelimiter start=%s%s%s ' .
          \ 'end=%s%s%s contains=@%s containedin=ALL',
          \ a:lang, z, b, z, z, e, z, a:lang)
  endfor

  if exists('csyn')
    let b:current_syntax = csyn
  endif
endfunction

function! s:init()
  let max_indent = get(g:, 'journal#max_indent', 10)
  let colors = s:extract_colors(max_indent)
  let shift = get(g:, 'journal#color_shift', max_indent / 2)
  for i in range(1, max_indent)
    let indent = i * &tabstop
    let allbut = 'ALLBUT,@NoSpell,topLevel,'.join(map(range(1, i), '"indent".v:val'), ',')
    execute printf('syn region indent%d start=/^\s\{%d}%s/           end=/^\(\s\{,%d}\S\)\@=/ contains=%s fold', i, indent, s:bullets, indent, allbut)
    execute printf('syn region indent%d start=/^\s\{%d}  \(%s\)\@<!/ end=/^\(\s\{,%d}\S\)\@=/ contains=%s fold', i, indent, s:bullets, indent, allbut)
    execute printf('syn match indentBullet%d /^\s\{%d,}\zs%s/ contains=@checkbox containedin=indent%d contained', i, indent, s:bullets, i)
    if !empty(colors)
      let cidx = i - 1 + shift
      let col  = colors[cidx % len(colors)]
      let bcol = colors[(cidx + max_indent / 2) % len(colors)]
      execute printf('hi indent%d %sfg=%s',       i, s:colfam(), col)
      execute printf('hi indentBullet%d %sfg=%s', i, s:colfam(), bcol)
    endif
  endfor

  syn spell toplevel
endfunction

augroup journal
  autocmd!
  autocmd ColorScheme * call s:init()
  if exists('##TextChangedI')
    autocmd TextChangedI <buffer> call s:detect('.', '.')
  else
    autocmd CursorMovedI <buffer> call s:detect('.', '.')
  endif
augroup END

call s:init()
call s:detect(1, '$')

let b:current_syntax = 'journal'

