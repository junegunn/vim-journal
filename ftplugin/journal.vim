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
  if i >= 0 && getline('.')[col('.') - 1:] =~ '^[-@#$*:xo0-9+>=][.:)]\?\s'
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

