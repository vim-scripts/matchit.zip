" -*- vim -*-
" vim:sts=2:sw=2:
" URL:  http://sites.netscape.net/BBenjiF/vim/beta/matchit.vim
" FILE: "D:\vim\beta\matchit.vim"
" UPLOAD: URL="ftp://siteftp.netscape.net/vim/beta/matchit.vim"
" UPLOAD: USER="bbenjif"
" LAST MODIFICATION: "Sun, 07 Jan 2001 22:48:31 Eastern Standard Time ()"
" (C) 2000 by Benji Fisher, <benji@member.AMS.org>
" $Id:$

" TODO:  for vim 6.0, I can use s: variables instead of wrapping the
" definition of b:match_words in a function.  I should also look into the new
" search() function and think about multi-line patterns for b:match_words.
" Other vim 6.0 features:  strpart(foo, 1), more regexp features, ...
" I should also convert the docs to vim help format.
" TODO:  Maybe I should add a menu so that people will actually use some of
" the features that I have implemented.

" I think the echo in the map clears the command line!
nmap% :<C-U>call Match_wrapper('', 1) \| echo '' <CR>
vmap% <Esc>%m'gv``
nmap g% :call Match_wrapper('', 0) \| echo '' <CR>
vmap g% <Esc>g%m'gv``

" Highlight group for error messages.  The user may choose to set this
" to be invisible.
hi link MatchError WarningMsg

function! Match_wrapper(word, forward) range
  if v:count
    exe "normal! " . v:count . "%"
    return
  elseif !exists("b:match_words") || b:match_words == ""
    normal! %
    return
  end
  " BR always means 'with back references.'
  " The first step is to create a search pattern with the BR's resolved.
  let notslash = '\(^\|[^\\]\)\(\\\\\)*'
  let patBR = substitute(b:match_words.":", '[,:]*:[,:]*', ':', 'g')
  let patBR = substitute(patBR, ',\{2,}', ',', 'g')
  let patBR = substitute(patBR, '\\@<=', '', 'g')
  if patBR !~ notslash . '\\\d'
    let do_BR = 0
    let pat = patBR
  else
    let do_BR = 1
    let pat = ""
    let maxlen = strlen(patBR)
    while patBR =~ '[^,:]'
    " call input(patBR)
      let i = match(patBR, ':') + 1
      let currentBR = strpart(patBR, 0, i-1) . ","
      let patBR = strpart(patBR, i, maxlen)
      let i = match(currentBR, ',') + 1
      let iniBR = strpart(currentBR, 0, i-1)
    " call input("cur#".currentBR."#pat#".patBR."#ini#".iniBR)
      let currentBR = strpart(currentBR, i, maxlen)
      let pat = pat . iniBR
      let i = match(currentBR, ',') + 1
      while i
	" In 'if,then,else', ini='if' and tail='then' and then tail='else'.
	let tailBR = strpart(currentBR, 0, i-1)
	let currentBR = strpart(currentBR, i, maxlen)
" call input("Match_resolve('" . iniBR . "', '" . tailBR . "', \"word\")")
	let pat = pat . "," . Match_resolve(iniBR, tailBR, "word")
" call input("Done!")
	let i = match(currentBR, ',') + 1
      endwhile " Now, currentBR has been used up.
      let pat = pat . ":"
    endwhile " patBR =~ '[^,:]'
  endif " patBR !~ '\\\d'
  if exists("b:match_debug")
    let b:match_pat = pat
  endif

  " Second step:  set match to the bit of text that matches one of the
  " patterns.  Require match to end on or after the cursor and prefer it to
  " start on or before the cursor.  The next several lines were here before
  " BF started messing with this script.
  " quote the special chars in 'matchpairs', replace [,:] with \| and then
  " append the builtin pairs (/*, */, #if, #ifdef, #else, #elif, #endif)
  let default = substitute(escape(&mps, '[$^.*~\\/?]'), '[,:]\+',
    \ '\\|', 'g').'\|\/\*\|\*\/\|#if\>\|#ifdef\>\|#else\>\|#elif\>\|#endif\>'
  " all = pattern with all the keywords
  let all = '\(' . substitute(pat, '[,:]\+', '\\|', 'g') . default . '\)'
  if a:word != ''
    " word given
    if a:word !~ all
      echohl MatchError|echo 'Missing rule for word:"'.a:word.'"'|echohl NONE
      return
    endif
    let match = a:word
    let matchline = match
    let prefix = '^\('
    let suffix = '\)$'
  " Now the case when "word" is not given
  elseif matchend(getline("."), '.*' . all) < col(".")
    " there is no special word in this line || it is before the cursor
    echohl MatchError | echo "No matching rule applies here" | echohl NONE
    return
  else	" Find the match that ends on or after the cursor and position the
	" cursor at the start of this match.
    let matchline = getline(".")
    let linelen = strlen(matchline)
    let curcol = col(".") " column of the cursor in match
    if curcol > 1
      let prefix = '^\(.\{,' . (curcol-1) . '}\)'
    else
      let prefix = '^\(\)'
    endif
    if curcol < linelen
      let suffix = '.\{,' . (linelen-curcol) . '}$'
    else
      let suffix = '$'
    endif
    " If there is no match including the cusor position, allow a match
    " that starts and ends after the cursor.
" call input("I am here! 1")
    if matchline !~ prefix.all.suffix
" call input("I am here! 2")
      " It might be better to have alternative "let curcol" and "let match"
      " lines here...
      let prefix = '^\(.\{-}\)'
    endif
    let curcol = strlen(substitute(matchline, prefix.all.suffix, '\1', ""))
    let match = substitute(matchline, prefix.all.suffix, '\2', "")
    if curcol
      let prefix = '^.\{' . curcol . '}\('
    else
      let prefix = '^\('
    endif
    if curcol + strlen(match) < linelen
      let suffix = '\).\{' . (linelen - curcol - strlen(match)) . '}$'
    else
      let suffix = '\)$'
    endif
    " Remove the trailing colon and replace '[,:]' with '\|':
    if matchline !~ prefix . substitute(strpart(pat, 0, strlen(pat)-1),
    \ '[,:]\+', '\\|', 'g') . suffix
      norm! %
      return
    endif
  endif
" call input("match=".match."# pat=".pat."#")
  if exists("b:match_debug")
    let b:match_match = match
    let b:match_col = curcol+1
  endif
" call input("I am here!")

  " Third step:  Find the group and single word that match, and the original
  " (backref) versions of these.  Then, resolve the backrefs.
  " Reconstruct the version with unresolved backrefs.
  let patWithAt = substitute(b:match_words.":", '[,:]*:[,:]*', ':', "g")
  let patWithAt = substitute(patWithAt, ',\{2,}', ',', "g")
  " Now, set pat and patWithAt to the matching group: 'if,endif' or
  " 'while,endwhile' or whatever.
  let i = Match_choose(pat, matchline, ":", ",", prefix, suffix)
  if i
    let pat = substitute(pat, '\([^:]*:\)\{' . i . '\}', '', '')
    let patWithAt = substitute(patWithAt, '\([^:]*:\)\{' . i . '\}', '', '')
  endif
  let pat = matchstr(pat, '[^:]*')
  let patWithAt = matchstr(patWithAt, '[^:]*')
" call input("i = " . i . " and pat = " . pat)
  if !do_BR
    " let pat = matchstr(pat, '[^:]*')
  else  " Do the hard part:  resolve those backrefs!
" call input("I am here:  1")
    let i = match(patWithAt, ',') + 1
    let iniWithAt = strpart(patWithAt, 0, i-1)
    let ini = substitute(iniWithAt, '\\@<=', '', 'g')
    let tail = strpart(patWithAt, i, strlen(patWithAt))
    if exists("b:match_debug")
      let b:match_iniBR = ini
      let b:match_wholeBR = patWithAt
    endif
    " Now, we do not need to keep pat, so we can extract the matching word
    " into pat.
    let i = Match_choose(pat, matchline, ",", "", prefix, suffix)
    if i
      let pat = substitute(pat, '\([^,]*,\)\{' . i . '\}', '', '')
      let patWithAt = substitute(patWithAt, '\([^,]*,\)\{' . i . '\}', '', '')
    endif
    let pat = matchstr(pat, '[^,]*')
    let patWithAt = matchstr(patWithAt, '[^,]*')
" call input("I am here:  1.2")
    " Now, match =~ pat and patWithAt is the version with backrefs.
    if patWithAt != iniWithAt
" call input("I am here:  1.3")
      let table = Match_resolve(ini, patWithAt, "table")
    else
" call input("I am here:  1.3")
      " let table = "0123456789"
      let table = ""
      let d = 0
      while d < 10
	if tail =~ notslash . '\\' . d
	  let table = table . d
	else
	  let table = table . "-"
	endif
	let d = d + 1
      endwhile
    endif
    let d = 9
" call input("I am here:  1.4")
    while d
      if table[d] != "-"
" call input("d=".d."  tail=" . tail)
	let backref = substitute(match, pat, '\'.table[d], "")
" Let's hope this doesn't cause any problems!  May 3, 2000:
	let backref = escape(backref, '*')
	let tail = substitute(tail, '\(' . notslash . '\)\\' .d,
	\ '\1' . escape(backref, '\\'), 'g')
" call input("backref=".backref."#d=".d)
" call input(Match_ref(ini, d, "start", "len"))
	" execute Match_ref(ini, d, "start", "len")
	" let ini = strpart(ini, 0, start) . backref
	" \ . strpart(ini, start+len, strlen(ini))
	execute Match_ref(iniWithAt, d, "start", "len")
	let iniWithAt = strpart(iniWithAt, 0, start) . backref
	\ . strpart(iniWithAt, start+len, strlen(iniWithAt))
" call input("d=".d."  tail=" . tail)
      endif
      let d = d-1
    endwhile
    let patWithAt = iniWithAt . "," . tail
    let pat = substitute(patWithAt, '\\@<=', '', 'g')
  endif " do_BR
" call input("I am here:  2")
  let i = match(pat, ",")
  let ini = strpart(pat, 0, i)
  let fin = substitute(pat, '.*,', '', '')
  let tail = strpart(pat, i+1, strlen(pat))
  let tail = substitute(tail, ',', '\\|', 'g')
  let bwtail = substitute(pat, ',[^,]*$', "", "")
  let bwtail = substitute(bwtail, ',', '\\|', 'g')
" call input("I am here:  pat = " . pat)
  if exists("b:match_debug")
    let b:match_ini = ini
    let b:match_tail = tail
    if do_BR
      let b:match_table = table
      let b:match_word = pat
    else
      let b:match_table = ""
      let b:match_word = ""
    endif
  endif

  " Fourth step:  actually start moving the cursor and call Match_Busca().
" call input("I am here! ini#".ini."#tail#".tail."#fin#".fin)
  " This minimizes screen jumps and avoids using a global mark.
  let restore_cursor = line(".") . "normal!" . virtcol(".") . "|"
  normal! H
  let restore_screen = line(".") . "normal!zt"
  execute restore_cursor
  " Later, :execute restore_screen to get to the original screen.
  if &ic
    let restore_options = "set ignorecase "
  else
    let restore_options = "set noignorecase "
  endif
  if exists("b:match_ignorecase")
    let &ignorecase = b:match_ignorecase
  endif
  if &ws
    let restore_options = restore_options . "wrapscan "
  else
    let restore_options = restore_options . "nowrapscan "
  endif
" call input("I am here:  4")
  set nows
  normal! 0
  if curcol
    execute "normal!" . curcol . "l"
  endif
  let v:errmsg = ''
" call input("match=".match."# fin=".fin."#")
  if a:forward && match =~ '^\(' . fin . '\)$'
    let v:errmsg = Match_Busca("?", fin, ini, ini)
  elseif a:forward
    let v:errmsg = Match_Busca("/", ini, fin, tail)
  elseif match =~ '^\(' . ini . '\)$'
    let v:errmsg = Match_Busca("/", ini, fin, fin)
  else
    let v:errmsg = Match_Busca("?", fin, ini, bwtail)
  endif
  " a hack to deal with "if...end if" situations
  " if getline(".")[col(".")-1] =~ '\s'
  "   normal! w
  " endif
  " In case pat = notend . 'if, else, endif'  and
  " patWithAt = notend . '\@<=if, else, endif'  and
  " the cursor is at the start of notend:
  if patWithAt =~ '\\@<='
" call input(patWithAt)
    let match = getline(".")
    let match = strpart(match, col(".")-1, strlen(match))
    let i = Match_choose(pat, match, ",", "", '^\(', '\)')
    if i > 0
      let patWithAt = substitute(patWithAt, '\(.\{-},\)\{'.i.'\}', '', '')
    endif
    let patWithAt = matchstr(patWithAt, '[^,]*')
    let i = match(patWithAt, '\\@<=') + 1
    if i
      let patWithAt = '\(' . strpart(patWithAt, 0, i-1) . '\)' .
      \ strpart(patWithAt, i+3, strlen(patWithAt))
      let offset = strlen(substitute(match, patWithAt.'.*', '\1', ''))
" let g:match = match
" let g:patWithAt = patWithAt
      if offset
	execute "normal!" . offset . "l"
      endif
    endif
  endif
  let final_position = line(".") . "normal!" . virtcol(".") . "|"
  " Restore options and original screen.
  execute restore_options
  execute restore_screen
  execute restore_cursor
  normal!m'
  if v:errmsg != -1 
    execute final_position
  endif
endfun

" dir: is either "/" or "?", defines the direction of the search
" ini: pattern for words that indicate the start of a group
" fin: pattern for words that indicate the end of a group
" tail: pattern for special words that are not the beginning of a nested group
"       for example: Match_Busca('/', '\<if\>', '\<end',
"		\ '\<elseif\>\|\<else\>\|\<end')
"       for example: Match_Busca('?', '\<end', '\<if\>', '\<if\>')
" note that if U R moving backwards ini='\<end', and fin='\<if\>'
function! Match_Busca(dir, ini, fin, tail) abort
" let g:foo=getline(".")."#".a:dir."#".a:ini."#".a:fin."#".a:tail."#"
  let ini = escape(a:ini, '/?')
  let fin = escape(a:fin, '/?')
  let tail = escape(a:tail, '/?')
  let pattern =  '\(' . ini . '\|' . tail . '\)'
  if a:dir == "?"
    let prefix = '.*'
  else
    let prefix = ''
  endif
  let string = getline(".")
  let start = col(".") - 1
  if start
    let end = matchend(string, '^.\{'.start.'}' . pattern)
  else
    let end = matchend(string, pattern)
  endif
  if exists("b:match_comment")
    let iscomment = 'strpart(string, 0, end) =~ b:match_comment'
    if exists("b:match_strings_like_comments")
      let iscomment = iscomment .
	\ '||synIDattr(synID(line("."),end,1),"name") =~? "string"'
" call input("iscomment:".iscomment)
    endif
  elseif has("syntax") && exists("g:syntax_on")
    let iscomment = 'synIDattr(synID(line("."),end,1),"name") =~? "comment"'
    if exists("b:match_strings_like_comments")
      let iscomment = substitute(iscomment, "ment", 'ment\\\\|string', "")
" call input("iscomment:".iscomment)
    endif
  else
    let iscomment = "0"
  endif
  execute 'let comment =' . iscomment
  if comment
    let iscomment = "0"
  endif
  let depth = 1 " nesting depth
" call input("depth=".depth." line=".line(".")." start=".start." end=".end)

  while depth
    if a:dir == "/"
      let end = matchend(string, '.\{' . end . '}' . pattern)
    else
      let start = match(strpart(string, 0, end), pattern . "$")
      let end = matchend(strpart(string, 0, start), '.*' . pattern)
    endif
" call input("depth=".depth." line=".line(".")." start=".start." end=".end)
    if end == -1
" call input("Here I am: 1; pattern=".pattern)
      execute a:dir . pattern
" call input("Here I am: 2")
      let string = getline(".")
      let end = matchend(string, prefix . pattern)
    endif
    execute 'let comment =' . iscomment
    if comment
	continue  " Comment:  no change to depth.
    endif
" call input("depth=".depth." line=".line(".")." start=".start." end=".end)
    " Maybe I should make sure to match '^' . ini . '$' .
    if  strpart(string, 0, end) =~ a:ini . "$"  " found ini
" call input(strpart(string, 0, end) ."#". a:ini)
      let depth = depth + 1
      if depth == 2
	let pattern =  '\(' . ini . '\|' . fin . '\)'
      endif
    else  " found fin or depth == 1 and found tail
      let depth = depth - 1
      if depth == 1
	let pattern =  '\(' . ini . '\|' . tail . '\)'
      endif
    endif
  endwhile

  normal! 0
  let start = match(strpart(string, 0, end), pattern . "$")
  if start
    execute "normal!" . start . "l"
  endif
endfunction

if 0
fun! Match_foo(d)
  let backref = '\(xx\(yy\)\)'
  execute Match_ref(backref, a:d, "start", "len")
  echo "start = " . start . " and len = " .len
endfun
endif

" No extra arguments:  Match_ref(string, d) will
" find the d'th occurrence of '\(' and return it, along with everything up
" to and including the matching '\)'.
" One argument:  Match_ref(string, d, "start") returns the index of the start
" of the d'th '\(' and any other argument returns the length of the group.
" Two arguments:  Match_ref(string, d, "foo", "bar") returns a string to be
" executed, having the effect of
"   :let foo = Match_ref(string, d, "start")
"   :let bar = Match_ref(string, d, "len")
fun! Match_ref(string, d, ...)
  let notslash = '\(^\|[^\\]\)\(\\\\\)*'
  let len = strlen(a:string)
  if a:d == 0
    let start = 0
  else
    let cnt = a:d
    let match = a:string
    while cnt
      let cnt = cnt - 1
      let index = matchend(match, notslash . '\\(')
      if index == -1
	return ""
      endif
      let match = strpart(match, index, len)
    endwhile
    let start = len - strlen(match)
"    let start = matchend(a:string, '\(.\{-}\\(\)\{' . a:d . '}')
"    if start == -1
"      return ""
    if a:0 == 1 && a:1 == "start"
      return start - 2
    endif
"    let match = strpart(a:string, start, len)
    let cnt = 1
    while cnt
      let index = matchend(match, notslash . '\\(\|\\)') - 1
      if index == -2
	return ""
      endif
      " Increment if an open, decrement if a ')':
      let cnt = match('0(', match[index]) + cnt
      let match = strpart(match, index+1, len)
    endwhile
    let start = start - 2
    let len = len - start - strlen(match)
  endif
  if a:0 == 1
    return len
  elseif a:0 == 2
    return "let " . a:1 . "=" . start . "| let " . a:2 . "=" . len
  else
    return strpart(a:string, start, len)
  endif
endfun

" Count the number of disjoint copies of pattern in string.
" If the pattern is a literal string and contains no '0' or '1' characters
" then Match_count(string, pattern, '0', '1') should be faster than
" Match_count(string, pattern).
fun! Match_count(string, pattern, ...)
" call input("string#".a:string."#pattern#".a:pattern)
  let pat = escape(a:pattern, '\\')
  if a:0 > 1
    let foo = substitute(a:string, '[^'.a:pattern.']', "a:1", "g")
    let foo = substitute(a:string, pat, a:2, "g")
    let foo = substitute(foo, '[^'.a:2.']', "", "g")
    return strlen(foo)
  endif
  let result = 0
  let foo = a:string
  let maxlen = strlen(foo)
  let index = matchend(foo, pat)
  while index != -1
    let result = result + 1
    let foo = strpart(foo, index, maxlen)
    let index = matchend(foo, pat)
  endwhile
  return result
endfun

" Match_resolve('\(a\)\(b\)', '\(c\)\2\1\1\2') should return table.word, where
" word = '\(c\)\(b\)\(a\)\3\2' and table = '-32-------'.  That is, the first
" '\1' in target is replaced by '\(a\)' in word, table[1] = 3, and this
" indicates that all other instances of '\1' in target are to be replaced
" by '\3'.  The hard part is dealing with nesting...
" Note that ":" is an illegal character for source and target.
fun! Match_resolve(source, target, output)
  let notslash = '\(^\|[^\\]\)\(\\\\\)*'
  let word = a:target
  let i = matchend(word, notslash . '\\\d') - 1
  let table = "----------"
  while i != -2 " There are back references to be replaced.
    let d = word[i]
    let backref = Match_ref(a:source, d)
    " The idea is to replace '\d' with backref.  Before we do this,
    " replace any \(\) groups in backref with :1, :2, ... if they
    " correspond to the first, second, ... group already inserted
    " into backref.  Later, replace :1 with \1 and so on.  The group
    " number w+b within backref corresponds to the group number
    " s within a:source.
    " w = number of '\(' in word before the current one
    let w = Match_count(
    \ substitute(strpart(word, 0, i-1), '\\\\', '', 'g'), '\(', '1')
    let b = 1 " number of the current '\(' in backref
    let s = d " number of the current '\(' in a:source
    while b <= Match_count(substitute(backref, '\\\\', '', 'g'), '\(', '1')
    \ && s < 10
      if table[s] == "-"
	if w + b < 10
	  " let table[s] = w + b
	  let table = strpart(table, 0, s) . (w+b) . strpart(table, s+1, 10)
	endif
	let b = b + 1
	let s = s + 1
      else
	execute Match_ref(backref, b, "start", "len")
	let ref = strpart(backref, start, len)
" call input("backref#".backref."# start=".start." len=".len)
	let backref = strpart(backref, 0, start) . ":". table[s]
	\ . strpart(backref, start+len, 1000000)
" call input("backref#".backref."# start=".start." len=".len)
	let s = s + Match_count(substitute(ref, '\\\\', '', 'g'), '\(', '1')
      endif
    endwhile
    let word = strpart(word, 0, i-1) . backref . strpart(word, i+1, 1000000)
    let i = matchend(word, notslash . '\\\d') - 1
  endwhile
  " echo "#".word."#".i."#".d | break
  let word = substitute(word, ':', '\\', 'g')
  if a:output == "table"
    return table
  elseif a:output == "word"
    return word
  else
    return table . word
  endif
endfun

" Match_choose("foo,bar,grok", "bar", ",", "", '\(', '\)')  should return 1.
fun! Match_choose(patterns, string, comma, branch, prefix, suffix)
  if a:patterns =~ a:comma . "$"
    let tail = a:patterns
  else
    let tail = a:patterns . a:comma
  endif
  if a:branch != ""
    let tail = substitute(tail, a:branch, '\\|', 'g')
  endif
  let maxlen = strlen(tail)
  let result = 0
  let i = match(tail, a:comma) + 1
  let current = strpart(tail, 0, i-1)
  while a:string !~ a:prefix . current . a:suffix
    let result = result + 1
    let tail = strpart(tail, i, maxlen)
    let i = match(tail, a:comma) + 1
    if i == 0
      return -1
    endif
    let current = strpart(tail, 0, i-1)
  endwhile
  return result
endfun

" Call this function to turn on debugging information.  Every time the main
" script is run, buffer variables will be saved.  These can be used directly
" or viewed using the menu items below.
command! -nargs=0 MatchDebug call Match_debug()
fun! Match_debug()
  let b:match_debug = 1	" Save debugging information.
  " pat = all of b:match_words with backrefs parsed
  amenu &Matchit.&pat	:echo b:match_pat<CR>
  " match = bit of text that is recognized as a match
  amenu &Matchit.&match	:echo b:match_match<CR>
  " curcol = cursor column of the start of the matching text
  amenu &Matchit.&curcol	:echo b:match_col<CR>
  " wholeBR = matching group, original version
  amenu &Matchit.wh&oleBR	:echo b:match_wholeBR<CR>
  " iniBR = 'if' piece, original version
  amenu &Matchit.ini&BR	:echo b:match_iniBR<CR>
  " ini = 'if' piece, with all backrefs resolved from match
  amenu &Matchit.&ini	:echo b:match_ini<CR>
  " tail = 'else\|endif' piece, with all backrefs resolved from match
  amenu &Matchit.&tail	:echo b:match_tail<CR>
  " fin = 'endif' piece, with all backrefs resolved from match
  amenu &Matchit.&word	:echo b:match_word<CR>
  " '\'.d in ini refers to the same thing as '\'.table[d] in word.
  amenu &Matchit.t&able	:echo '0:' . b:match_table . ':9'<CR>
endfun

" The following autocommands are wrapped in a function, to be used once when
" this file is sourced, in order to allow the use of local variables.  The
" function is called and then deleted after it is defined.
fun Match_autocommands()
  aug Matchit
    au!
    " Ada:  thanks to Neil Bird.
    let notend = '\(^\s*\|[^d\t ]\s\+\)'
    execute "au FileType ada let b:match_words='" .
    \ notend . '\@<=\<if\>,\<elsif\>,\<else\>,\<end\>\s\+\<if\>:' .
    \ notend . '\@<=\<case\>,\<when\>,\<end\>\s\+\<case\>:' .
    \ '\(\<while\>.*\|\<for\>.*\|'.notend.'\)\<loop\>,\<end\>\s\+\<loop\>:' .
    \ '\(\<do\>\|\<begin\>\),\<exception\>,\<end\>\s*\($\|[;A-Z]\):' .
    \ notend . '\@<=\<record\>,\<end\>\s\+\<record\>' .
    \ "'"
    " ASP:  Active Server Pages (with Visual Basic Script)
    " thanks to Gontran BAERTS
    execute "au FileType aspvbs do Matchit FileType html |" .
    \ "let b:match_words ='" .
    \ notend . '\@<=\<If\>,\<Else\>,\<ElseIf\>,\<end\s\+\<if\>:' .
    \ notend . '\@<=\<Select\s\+\<Case\>,\<Case\>,\<Case\s\+\<Else\>,' .
    \	'\<End\s\+\<Select\>:' .
    \ '^\s*\<Sub\>,\<End\s\+\<Sub\>:' .
    \ '^\s*\<Function\>,\<End\s\+\<Function\>:' .
    \ '\<Class\>,\<End\s\+\<Class\>:' .
    \ '^\s*\<Do\>,\<Loop\>:' .
    \ '^\s*\<For\>,\<Next\>:' .
    \ '\<While\>,\<Wend\>:' .
    \ "' . b:match_words"
    " Csh:  thanks to Johannes Zellner
    " - Both foreach and end must appear alone on separate lines.
    " - The words else and endif must appear at the beginning of input lines;
    "   the if must appear alone on its input line or after an else.
    " - Each case label and the default label must appear at the start of a
    "   line.
    " - while and end must appear alone on their input lines.
    au FileType csh,tcsh let b:match_words =
	\ '^\s*\<if\>.*(.*).*\<then\>,'.
	\   '^\s*\<else\>\s\+\<if\>.*(.*).*\<then\>,^\s*\<else\>,'.
	\   '^\s*\<endif\>:'.
	\ '\(^\s*\<foreach\>\s\+\S\+\|^s*\<while\>\).*(.*),'.
	\   '\<break\>,\<continue\>,^\s*\<end\>:'.
	\ '^\s*\<switch\>.*(.*),^\s*\<case\>\s\+,^\s*\<default\>,^\s*\<endsw\>'
    " DTD:  thanks to Johannes Zellner
    " - match <!--, --> style comments.
    " - match <! with >
    " - TODO:  why does '--,--:'. not work ?
    au! FileType dtd let b:match_words =
      \ '<!--,-->:'.
      \ '<!,>'
    " Entity:  see XML.
    " Essbase:
    au BufNewFile,BufRead *.csc let b:match_words=
    \ '\<fix\>,\<endfix\>:' .
    \ '\<if\>,\<else\(if\)\=\>,\<endif\>:' .
    \ '\<!loopondimensions\>\|\<!looponselected\>,\<!endloop\>'
    " Fortran:  thanks to Johannes Zellner
    au FileType fortran let b:match_words =
      \ 'do\s\+\([0-9]\+\),^\s*\1\s:'.
      \ '^[0-9 \t]\+\<if.*then\>,\<else\s*\(if.*then\)\=\>,\<endif\>'
    " HTML:  thanks to Johannes Zellner.
    au FileType html,jsp let b:match_ignorecase = 1 |
      \ let b:match_words = '<,>:' .
      \ '<\@<=[ou]l[^>]*\(>\|$\),<\@<=li>,<\@<=/[ou]l>:' .
      \ '<\@<=\([^/][^ \t>]*\)[^>]*\(>\|$\),<\@<=/\1>'
    " JSP:  see HTML
    " LaTeX:
    au FileType tex let b:match_ignorecase = 0
      \ | let b:match_comment = '\(^\|[^\\]\)\(\\\\\)*%'
      \ | let b:match_words = '(,):\[,]:{,}:\\(,\\):\\\[,\\]:' .
      \ '\\begin\s*\({\a\+\*\=}\),\\end\s*\1'
    " Lua version 4.0+:  thanks to Max Ischenko
    au FileType lua let b:match_ignorecase = 0 | let b:match_words =
      \ '\<\(do\|function\|if\)\>,' .
      \ '\<\(return\|else\|elseif\)\>,' .
      \ '\<end\>:' .
      \ '\<repeat\>,\<until\>'
    " Pascal:
    au FileType pascal let b:match_words='\<begin\>,\<end\>'
    " SGML:  see XML
    " Shell:  thanks to Johannes Zellner
    au FileType sh,config let b:match_words =
	\ '^\s*\<if\>\|;\s*\<if\>,'.
	\   '^\s*\<elif\>\|;\s*\<elif\>,^\s*\<else\>\|;\s*\<else\>,'.
	\   '^\s*\<fi\>\|;\s*\<fi\>:'.
	\ '^\s*\<for\>\|;\s*\<for\>\|^\s*\<while\>\|;\s*\<while\>,'.
	\   '^\s*\<done\>\|;\s*\<done\>:'.
	\ '^\s*\<case\>\|;\s*\<case\>,^\s*\<esac\>\|;\s*\<esac\>'
  " RPM Spec:  thanks to Max Ischenko
  au FileType spec let b:match_ignorecase = 0 | let b:match_words = 
    \ '^Name,^%description,^%clean,^%setup,^%build,^%install,^%files,' .
    \ '^%package,^%preun,^%postun,^%changelog'
    " Tcsh:  see Csh
    " TeX:  see LaTeX; I do not think plain TeX needs this.
    " Verilog:  thanks to Mark Collett
    au FileType verilog let b:match_ignorecase = 0
	\ | let b:match_words = 
	\ '\<begin\>,\<end\>:'.
	\ '\<case\>\|\<casex\>\|\<casez\>,\<endcase\>:'.
	\ '\<module\>,\<endmodule\>:'.
	\ '\<function\>,\<endfunction\>:'.
	\ '`ifdef\>,`else\>,`endif\>'
    " Vim:
    au FileType vim let b:match_ignorecase=0 |
    \ let b:match_strings_like_comments = 1 |
    \ let b:match_words=
    \ '\<fun\(c\=\|cti\=\|ction\=\)\>,\<retu\(rn\=\)\>\=,' .
    \   '\<endf\(u\=\|unc\=\|uncti\=\|unction\=\)\>:' .
    \ '\<while\>,\<break\>,\<con\k*\>,\<endw\k*\>:' .
    \ '\<if\>,\<el\(s\=\|sei\=\|seif\)\>,\<en\(d\=\|dif\=\)\>:' .
    \ '\<aug\k*\s\+\([^E]\|E[^N]\|EN[^D]\),\<aug\k*\s\+END\>:' .
    \ '(,)'
    " XML:  thanks to Johannes Zellner and Akbar Ibrahim
    " - case sensitive
    " - don't match empty tags <fred/>
    " - match <!--, --> style comments (but not --, --)
    " - match <!, > inlined dtd's. This is not perfect, as it
    "   gets confused for example by
    "       <!ENTITY gt ">">
    au! FileType xml,sgml,entity let b:match_ignorecase=0 | let b:match_words =
      \ '<,>:' .
      \ '<\@<=!\[CDATA\[,]]>:'.
      \ '<\@<=!--,-->:'.
      \ '<\@<=?\k\+,?>:'.
      \ '<\@<=\([^ \t>/]\+\)\(\s\+[^>]*\([^/]>\|$\)\|>\|$\),<\@<=/\1>:'.
      \ '<\@<=\([^ \t>/]\+\)\(\s\+[^/>]*\|$\),/>'

  aug END
endfun

call Match_autocommands()
delfunction Match_autocommands
