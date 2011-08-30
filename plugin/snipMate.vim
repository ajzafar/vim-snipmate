" File:          snipMate.vim
" Author:        Michael Sanders
" Version:       0.84
" Description:   snipMate.vim implements some of TextMate's snippets features in
"                Vim. A snippet is a piece of often-typed text that you can
"                insert into your document using a trigger word followed by a "<tab>".
"
"                For more help see snipMate.txt; you can do this by using:
"                :helptags ~/.vim/doc
"                :h snipMate.txt

if exists('loaded_snips') || &cp || version < 700
	finish
endif
let loaded_snips = 1
if !exists('snips_author') | let snips_author = 'Me' | endif

" save and reset 'cpo'
let s:save_cpo = &cpo
set cpo&vim

let s:snips = {}
if !exists('snippets_dir')
	let snippets_dir = substitute(globpath(&rtp, 'snippets/'), "\n", ',', 'g')
endif

" autocmds, maps, commands {{{

augroup snipmate
	au BufRead,BufNewFile *.snippets\= set ft=snippet
	au FileType snippet setl noet fdm=expr fde=getline(v:lnum)!~'^\\t\\\\|^$'?'>1':1
	au VimEnter * call s:CreateSnippets(['_'])
	au FileType * if &ma | call s:CreateSnippets(split(&ft, '\.')) | endif
augroup END

inoremap <silent> <Plug>snipmateTrigger  <C-R>=<SID>TriggerSnippet()<CR>
inoremap <silent> <Plug>snipmatePrev     <C-R>=<SID>PrevTabStop()<CR>
inoremap <silent> <Plug>snipmateNext     <C-R>=<SID>NextTabStop()<CR>
inoremap <silent> <Plug>snipmateShow     <C-R>=<SID>ShowAvailableSnips()<CR>
smap     <silent> <Plug>snipmateNext     <Esc>a<Plug>snipmateNext
smap     <silent> <Plug>snipmatePrev     <Esc>a<Plug>snipmatePrev

imap <Tab>      <Plug>snipmateTrigger
imap <S-Tab>    <Plug>snipmatePrev
imap <C-R><Tab> <Plug>snipmateShow
smap <Tab>      <Plug>snipmateNext
smap <S-Tab>    <Plug>snipmatePrev

command! -complete=filetype -nargs=* -bar
			\ ReloadSnippets call s:ReloadSnippets(<f-args>)
command! -complete=filetype -nargs=* -bar
			\ ResetSnippets call s:ResetSnippets(<f-args>)

" }}}
" Snippet creation {{{

let s:did_ft = {}
function! s:CreateSnippets(...)
	let scopes = a:0 ? a:1 : s:GetScopes()
	for ft in scopes
		if has_key(s:did_ft, ft) | continue | endif
		if !has_key(s:snips, ft) | let s:snips[ft] = {} | endif
		call s:DefineSnips(ft)
		let s:did_ft[ft] = 1
	endfor
endfunction

function! s:DefineSnips(scope)
	let dir = g:snippets_dir
	for path in split(globpath(dir, a:scope.'.snippets'), "\n") +
				\ split(globpath(dir, a:scope.'/*.snippets'), "\n")
		call s:ExtractSnipsFile(path, a:scope)
	endfor
endfunction

function! s:ExtractSnipsFile(file, scope)
	if !filereadable(a:file) | return | endif
	let text = readfile(a:file)
	let inSnip = 0
	let valid = 1
	for line in text + ["\n"]
		if inSnip
			if (line[0] == "\t" || line == '')
				let content .= strpart(line, 1)."\n"
				continue
			elseif valid
				call s:MakeSnip(a:scope, trigger, content[:-2], desc)
			endif
			let inSnip = 0
		endif

		if line[:6] == 'snippet'
			let inSnip = 1
			let valid = 1
			let trigger = strpart(line, 8)
			let desc = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process snip
				let desc = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''

			if trigger !~ s:GetTriggerRegex('$', '^')
				echom 'Invalid snippet trigger' trigger
				let valid = 0
			endif
		elseif line[:6] =~# 'extends'
			call s:ExtendScope(a:scope, split(strpart(line, 8), ','))
		endif
	endfor
endfunction

function! s:MakeSnip(scope, trigger, content, desc)
	if !has_key(s:snips[a:scope], a:trigger)
		let s:snips[a:scope][a:trigger] = [[a:desc, a:content]]
	else
		let s:snips[a:scope][a:trigger] += [[a:desc, a:content]]
	endif
endfunction

function! s:ExtendScope(real_scope, aliases)
	for alias in a:aliases
		call s:CreateSnippets([alias])
		for trigger in keys(get(s:snips, alias, {}))
			if !has_key(s:snips[a:real_scope], trigger)
				let s:snips[a:real_scope][trigger] = []
			endif
			call extend(s:snips[a:real_scope][trigger], s:snips[alias][trigger])
		endfor
	endfor
endfunction

" }}}
" Snippet triggering/expansion {{{

function! s:TriggerSnippet()
	if exists('s:tab_stops')
		let jump = s:NextTabStop()
		if type(jump) == 1
			return jump
		endif
	endif

	" Grab the trigger (and where it begins)
	let [trigger, begin] = s:GrabTrigger()
	" See if we have a snippet for that
	for scope in s:GetScopes()
		let snippet = s:GetSnippet(trigger, scope)

		" Found one, remove the trigger and expand the snippet
		if snippet != ''
			let &undolevels = &undolevels
			sil exe 's/\V'.escape(trigger, '/\').'\%#//'
			return snipMate#expandSnip(snippet, begin)
		endif
	endfor

	" haven't found one, get some matches
	let matches = s:GetMatches(trigger)
	" no matches? insert a tab
	if empty(matches) || trigger == ''
		return "\<tab>"
	endif
	call complete(begin, matches)

	" only one possible match, trigger it
	if len(matches) == 1
		return s:TriggerSnippet()
	else
	" return nothing otherwise we break the completion
		return ''
	end
endfunction

function! s:GrabTrigger()
	let t = matchstr(getline('.'), s:GetTriggerRegex('\%' . col('.') . 'c'))
	return [t, col('.') - len(t)]
endfunction

function! s:GetMatches(trigger)

	" get possible snippets
	let snippets = []
	for scope in split(&ft, '\.') + ['_']
		for key in keys(get(s:snips, scope, {}))
			let i = 1
			for description in s:snips[scope][key]
				let item = {}
				let item['word'] = key . '_' . i
				let item['abbr'] = key
				let item['menu'] = len(description[0]) ? description[0] :
							\ description[1]
				let item['dup'] = '1'
				call insert(snippets, item)
				let i += 1
			endfor
		endfor
	endfor
	call filter(snippets, 'v:val.word =~ "\\v^' . escape(a:trigger, '"\') . '"')
	call sort(snippets)
	return snippets
endfunction

" Check if trigger is the only usable trigger
function! s:GetSnippet(trigger, scope)
	if a:trigger =~ '_\d\+$'
		let [trigger, id] = split(a:trigger, '_')
	else
		let trigger = a:trigger
		let id = ''
	endif

	let snippets = get(s:snips[a:scope], trigger, [])
	return id || len(snippets) == 1 ? snippets[id - 1][1] : ''
endfunction

" }}}
" Misc/Utility {{{

function! s:ShowAvailableSnips()
	let [trigger, begin] = s:GrabTrigger()
	let matches = s:GetMatches(trigger)
	call complete(begin, matches)
	return ''
endfunction

function! s:GetTriggerRegex(end, ...)
	let begin = a:0 ? a:1 : ''

	" Valid snippet triggers follow the same rules as abbrevations.
	" See :h abbreviations
	" Hopefully someday this regex will be nicer.
	" Match full-id
	let re = begin . '\k\+\%(_\d\)\?' . a:end . '\|'
	" Match end-id. The group uses /\& to match a non-keyword character
	let re .= begin . '\%(\k\@!\&\S\)\+\k\%(_\d\)\?' . a:end . '\|'
	" Match non-id
	let re .= begin . '\S*\%(\k\@!\&\S\)\%(_\d\)\?' . a:end

	return re
endfunction

function! s:GetScopes()
	return split(&ft, '\.') + ['_']
endfunction

function! s:PrevTabStop()
	if exists('s:tab_stops') | return snipMate#jumpTabStop(1) | endif
	return "\<s-tab>"
endfunction

function! s:NextTabStop()
	return snipMate#jumpTabStop(0)
endfunction

" Reload snippets for filetype.
function! s:ReloadSnippets(...)
	let scopes = a:0 ? a:000 : s:GetScopes()
	call call('s:ResetSnippets', scopes)
	call s:CreateSnippets(scopes)
endfunction

" Reset snippets for filetype.
function! s:ResetSnippets(...)
	let scopes = a:0 ? a:000 : s:GetScopes()
	for dict in [s:snips, s:did_ft]
		for scope in scopes
			unlet! dict[scope]
		endfor
	endfor
endfunction

" }}}
" autoload {{{

function! s:RemoveSnippet()
	unl! s:tab_stops s:cur_stop s:snipLen s:endCol s:endLine s:prevLen s:oldWord
	if exists('s:has_mirrors')
		unl s:startCol s:origWordLen s:has_mirrors
		if exists('s:oldVars') | unl s:oldVars s:oldEndCol | endif
	endif
	aug! snipMateAutocmds
endfunction

function! snipMate#expandSnip(snip, col)
	let lnum = line('.') | let col = a:col

	let snippet = s:ProcessSnippet(a:snip)
	" Avoid error if eval evaluates to nothing
	if snippet == '' | return '' | endif

	" Expand snippet onto current position with the tab stops removed
	let snipLines = split(s:RemoveTabStops(substitute(snippet, '$\d\+', '', 'g')), "\n", 1)

	let line = getline(lnum)
	let afterCursor = strpart(line, col - 1)
	" Keep text after the cursor
	if afterCursor != "\t" && afterCursor != ' '
		let line = strpart(line, 0, col - 1)
		let snipLines[-1] .= afterCursor
	else
		let afterCursor = ''
		" For some reason the cursor needs to move one right after this
		if line != '' && col == 1 && &ve != 'all' && &ve != 'onemore'
			let col += 1
		endif
	endif

	" Insert snippet with proper indentation
	let indent = indent(lnum) + 1
	call setline(lnum, line.snipLines[0])
	call append(lnum, map(snipLines[1:], "'".strpart(line, 0, indent - 1)."'.v:val"))

	" Open any folds snippet expands into
	if &fen | sil! exe lnum.','.(lnum + len(snipLines) - 1).'foldopen' | endif

	let [s:tab_stops, s:snipLen] = s:BuildTabStops(snippet, lnum, col - indent, indent)

	if s:snipLen
		aug snipMateAutocmds
			au CursorMovedI * call s:UpdateChangedSnip(0)
			au InsertEnter * call s:UpdateChangedSnip(1)
		aug END
		let s:cur_stop = 0
		let s:endCol = s:tab_stops[s:cur_stop][1]
		let s:endLine = s:tab_stops[s:cur_stop][0]

		call cursor(s:tab_stops[s:cur_stop][0], s:tab_stops[s:cur_stop][1])
		let s:prevLen = col('$')
		if s:tab_stops[s:cur_stop][2] != -1 | return s:SelectWord() | endif
	else
		unl s:tab_stops s:snipLen
		" Place cursor at end of snippet if no tab stop is given
		let newlines = len(snipLines) - 1
		call cursor(lnum + newlines, indent + len(snipLines[-1]) - len(afterCursor)
					\ + (newlines ? 0: col - 1))
	endif
	return ''
endfunction

" Prepare snippet to be processed by s:BuildTabStops
function! s:ProcessSnippet(snip)
	let snippet = a:snip
	" Evaluate eval (`...`) expressions.
	" Backquotes prefixed with a backslash "\" are ignored.
	" And backslash can be escaped by doubling it.
	" Using a loop here instead of a regex fixes a bug with nested "\=".
	if stridx(snippet, '`') != -1
		let new = []
		let snip = split(snippet, '\%(\\\@<!\%(\\\\\)*\)\@<=`', 1)
		let isexp = 0
		for i in snip
			if isexp
				call add(new, substitute(eval(i), "\n\\%$", '', ''))
			else
				call add(new, i)
			endif
			let isexp = !isexp
		endfor
		let snippet = join(new, '')
		let snippet = substitute(snippet, "\r", "\n", 'g')
		let snippet = substitute(snippet, '\\`', "`", 'g')
		let snippet = substitute(snippet, '\\\\', "\\", 'g')
	endif

	" Update a:snip so that all the $# become the text after the colon in their
	" associated ${#}.
	" (e.g. "${1:foo}" turns all "$1"'s into "foo")
	let i = 1
	while stridx(snippet, '${'.i) != -1
		let s = s:GetPlaceholder(snippet, i)
		if s != ''
			" Take care of nested placeholders by removing the surrounding ${:}
			let s = s:RemoveTabStops(s)
			let snippet = substitute(snippet, '$'.i, s.'&', 'g')
		endif
		let i += 1
	endw

	" Add ${0} tab stop if found
	if stridx(snippet, '${0') != -1
		let snippet = substitute(snippet, '${0', '${'.i, '')
		let s = matchstr(snippet, '${'.i.':\zs.\{-}\ze}')
		if s != ''
			let snippet = substitute(snippet, '$0', '$'.i, 'g')
			let snippet = substitute(snippet, '$'.i, s.'&', 'g')
		endif
	else
		let snippet .= '${'.i.'}'
	endif

	if &et " Expand tabs to spaces if 'expandtab' is set.
		return substitute(snippet, '\t', repeat(' ', &sts ? &sts : &sw), 'g')
	endif
	return snippet
endfunction

" Removes tab stops, leaving only its placeholder; e.g., "${1:baz}" becomes
" simply "baz".
function! s:RemoveTabStops(snippet)
	let snippet = a:snippet
	while match(snippet, '${\d\+') != -1
		let snippet = substitute(snippet, '${\d\+:\=\([^{}]\{-}\)}', '\1', 'g')
	endw
	return snippet
endfunction

" Removes all tab stops except the one specified.
function! s:RemoveAllExcept(snippet, skip)
	let i = 1
	let snippet = a:snippet
	while stridx(snippet, '${'.i) != -1
		if i != a:skip
			let s = s:GetPlaceholder(snippet, i)
			let snippet = s == ''
						\ ? substitute(snippet, '${'.i.'}', '', '')
			            \ : substitute(snippet, '${'.i.':\('.s.'\)}', '\1', '')
		endif
		let i += 1
	endw
	return snippet
endfunction

" Returns the placeholder for a given tabstop, nested placeholders and all.
function! s:GetPlaceholder(snippet, tabstop)
	let portion = matchstr(a:snippet, '${'.a:tabstop.':.*}')
	let portion = strpart(portion, 0, s:SearchPair(portion, '{', '}'))
	return strpart(portion, stridx(portion, ':') + 1)
endfunction

" Returns the end of the nested start-end pair in the given string.
function! s:SearchPair(string, start, end)
	let start = stridx(a:string, a:start)
	let end = -1
	while 1
		let start = stridx(a:string, a:start, start + 1)
		let end = stridx(a:string, a:end, end + 1)
		if start == -1 || end < start
			return end
		endif
	endw
endfunction

" Counts occurences of haystack in needle
function! s:Count(haystack, needle)
	let counter = 0
	let index = stridx(a:haystack, a:needle)
	while index != -1
		let index = stridx(a:haystack, a:needle, index+1)
		let counter += 1
	endw
	return counter
endfunction

" Builds a list of a list of each tab stop in the snippet containing:
" 1.) The tab stop's line number.
" 2.) The tab stop's column number
"     (by getting the length of the string between the last "\n" and the
"     tab stop).
" 3.) The length of the text after the colon for the current tab stop
"     (e.g. "${1:foo}" would return 3). If there is no text, -1 is returned.
" 4.) If the "${#:}" construct is given, another list containing all
"     the matches of "$#", to be replaced with the placeholder. This list is
"     composed the same way as the parent; the first item is the line number,
"     and the second is the column.
function! s:BuildTabStops(snip, lnum, col, indent)
	let snipPos = []
	let i = 1
	let withoutVars = substitute(a:snip, '$\d\+', '', 'g')
	let noTabStops = s:RemoveTabStops(a:snip)
	while stridx(a:snip, '${'.i) != -1
		let beforeTabStop = matchstr(withoutVars, '^.*\ze${'.i.'\D')
		let withoutOthers = s:RemoveAllExcept(withoutVars, i)

		let j = i - 1
		call add(snipPos, [0, 0, -1])
		let snipPos[j][0] = a:lnum + s:Count(beforeTabStop, "\n")
		let snipPos[j][1] = a:indent + len(matchstr(withoutOthers, '.*\(\n\|^\)\zs.*\ze${'.i))
		if snipPos[j][0] == a:lnum | let snipPos[j][1] += a:col | endif

		" Get all $# matches in another list, if ${#:name} is given
		if stridx(withoutVars, '${'.i.':') != -1
			let snipPos[j][2] = strwidth(s:RemoveTabStops(s:GetPlaceholder(withoutVars, i)))
			let dots = repeat('.', snipPos[j][2])
			call add(snipPos[j], [])

			let withoutOthers = substitute(noTabStops, '$'.i.'\@!\d\+', '', 'g')
			while match(withoutOthers, '$'.i.'\(\D\|$\)') != -1
				let beforeMark = matchstr(withoutOthers, '^.\{-}\ze'.dots.'$'.i.'\(\D\|$\)')
				call add(snipPos[j][3], [0, 0])
				let snipPos[j][3][-1][0] = a:lnum + s:Count(beforeMark, "\n")
				let snipPos[j][3][-1][1] = a:indent + (snipPos[j][3][-1][0] > a:lnum
				                           \ ? len(matchstr(beforeMark, '.*\n\zs.*'))
				                           \ : a:col + len(beforeMark))
				let withoutOthers = substitute(withoutOthers, '$'.i.'\ze\(\D\|$\)', '', '')
			endw
		endif
		let i += 1
	endw
	return [snipPos, i - 1]
endfunction

function! snipMate#jumpTabStop(backwards)
	let leftPlaceholder = exists('s:origWordLen')
	                      \ && s:origWordLen != s:tab_stops[s:cur_stop][2]
	if leftPlaceholder && exists('s:oldEndCol')
		let startPlaceholder = s:oldEndCol + 1
	endif

	if exists('s:has_mirrors')
		call s:UpdatePlaceholderTabStops()
	else
		call s:UpdateTabStops()
	endif

	" Don't reselect placeholder if it has been modified
	if leftPlaceholder && s:tab_stops[s:cur_stop][2] != -1
		if exists('startPlaceholder')
			let s:tab_stops[s:cur_stop][1] = startPlaceholder
		else
			let s:tab_stops[s:cur_stop][1] = col('.')
			let s:tab_stops[s:cur_stop][2] = 0
		endif
	endif

	let s:cur_stop += a:backwards ? -1 : 1
	" Loop over the snippet when going backwards from the beginning
	if s:cur_stop < 0 | let s:cur_stop = s:snipLen - 1 | endif

	if exists('s:nested_count') " If a nested placeholder has been added, skip past it.
		let s:cur_stop += s:nested_count
		unl s:nested_count
	endif
	if s:cur_stop == s:snipLen
		let sMode = s:endCol == s:tab_stops[s:cur_stop-1][1]+s:tab_stops[s:cur_stop-1][2]
		call s:RemoveSnippet()
		return sMode ? "\<tab>" : -1
	endif

	call cursor(s:tab_stops[s:cur_stop][0], s:tab_stops[s:cur_stop][1])

	let s:endLine = s:tab_stops[s:cur_stop][0]
	let s:endCol = s:tab_stops[s:cur_stop][1]
	let s:prevLen = col('$')

	return s:tab_stops[s:cur_stop][2] == -1 ? '' : s:SelectWord()
endfunction

function! s:UpdatePlaceholderTabStops()
	let changeLen = s:origWordLen - s:tab_stops[s:cur_stop][2]
	unl s:startCol s:origWordLen s:has_mirrors
	if !exists('s:oldVars') | return | endif
	" Update tab stops in snippet if text has been added via "$#"
	" (e.g., in "${1:foo}bar$1${2}").
	if changeLen != 0
		let curLine = line('.')

		for pos in s:tab_stops
			if pos == s:tab_stops[s:cur_stop] | continue | endif
			let changed = pos[0] == curLine && pos[1] > s:oldEndCol
			let changedVars = 0
			let endPlaceholder = pos[2] - 1 + pos[1]
			" Subtract changeLen from each tab stop that was after any of
			" the current tab stop's placeholders.
			for [lnum, col] in s:oldVars
				if lnum > pos[0] | break | endif
				if pos[0] == lnum
					if pos[1] > col || (pos[2] == -1 && pos[1] == col)
						let changed += 1
					elseif col < endPlaceholder
						let changedVars += 1
					endif
				endif
			endfor
			let pos[1] -= changeLen * changed
			let pos[2] -= changeLen * changedVars " Parse variables within placeholders
                                                  " e.g., "${1:foo} ${2:$1bar}"

			if pos[2] == -1 | continue | endif
			" Do the same to any placeholders in the other tab stops.
			for nPos in pos[3]
				let changed = nPos[0] == curLine && nPos[1] > s:oldEndCol
				for [lnum, col] in s:oldVars
					if lnum > nPos[0] | break | endif
					if nPos[0] == lnum && nPos[1] > col
						let changed += 1
					endif
				endfor
				let nPos[1] -= changeLen * changed
			endfor
		endfor
	endif
	unl s:endCol s:oldVars s:oldEndCol
endfunction

function! s:UpdateTabStops()
	let changeLine = s:endLine - s:tab_stops[s:cur_stop][0]
	let changeCol = s:endCol - s:tab_stops[s:cur_stop][1]
	if exists('s:origWordLen')
		let changeCol -= s:origWordLen
		unl s:origWordLen
	endif
	let lnum = s:tab_stops[s:cur_stop][0]
	let col = s:tab_stops[s:cur_stop][1]
	" Update the line number of all proceeding tab stops if <cr> has
	" been inserted.
	if changeLine != 0
		let changeLine -= 1
		for pos in s:tab_stops
			if pos[0] >= lnum
				if pos[0] == lnum | let pos[1] += changeCol | endif
				let pos[0] += changeLine
			endif
			if pos[2] == -1 | continue | endif
			for nPos in pos[3]
				if nPos[0] >= lnum
					if nPos[0] == lnum | let nPos[1] += changeCol | endif
					let nPos[0] += changeLine
				endif
			endfor
		endfor
	elseif changeCol != 0
		" Update the column of all proceeding tab stops if text has
		" been inserted/deleted in the current line.
		for pos in s:tab_stops
			if pos[1] >= col && pos[0] == lnum
				let pos[1] += changeCol
			endif
			if pos[2] == -1 | continue | endif
			for nPos in pos[3]
				if nPos[0] > lnum | break | endif
				if nPos[0] == lnum && nPos[1] >= col
					let nPos[1] += changeCol
				endif
			endfor
		endfor
	endif
endfunction

function! s:SelectWord()
	let s:origWordLen = s:tab_stops[s:cur_stop][2]
	let s:oldWord = strpart(getline('.'), s:tab_stops[s:cur_stop][1] - 1,
				\ s:origWordLen)
	let s:prevLen -= s:origWordLen
	if !empty(s:tab_stops[s:cur_stop][3])
		let s:has_mirrors = 1
		let s:endCol = -1
		let s:startCol = s:tab_stops[s:cur_stop][1] - 1
	endif
	if !s:origWordLen | return '' | endif
	let l = col('.') != 1 ? 'l' : ''
	if &sel == 'exclusive'
		return "\<esc>".l.'v'.s:origWordLen."l\<c-g>"
	endif
	return s:origWordLen == 1 ? "\<esc>".l.'gh'
							\ : "\<esc>".l.'v'.(s:origWordLen - 1)."l\<c-g>"
endfunction

" This updates the snippet as you type when text needs to be inserted
" into multiple places (e.g. in "${1:default text}foo$1bar$1",
" "default text" would be highlighted, and if the user types something,
" UpdateChangedSnip() would be called so that the text after "foo" & "bar"
" are updated accordingly)
"
" It also automatically quits the snippet if the cursor is moved out of it
" while in insert mode.
function! s:UpdateChangedSnip(entering)
	" If tab stop has been modified, delete any nested placeholders it has.
	if exists('s:origWordLen') && !exists('s:nested_count')
	                         \ && col('$') - (s:prevLen + s:origWordLen)
		call s:DeleteNestedPlaceholders()
	endif

	if exists('s:has_mirrors') " If modifying a placeholder
		if !exists('s:oldVars') && s:cur_stop + 1 < s:snipLen
			" Save the old snippet & word length before it's updated.
			" s:startCol must be saved too, in case text is added
			" before the snippet (e.g. in "foo$1${2}bar${1:foo}").
			let s:oldEndCol = s:startCol
			let s:oldVars = deepcopy(s:tab_stops[s:cur_stop][3])
		endif
		let col = col('.') - 1

		if s:endCol != -1
			let changeLen = col('$') - s:prevLen
			let s:endCol += changeLen
		else " When being updated the first time, after leaving select mode
			if a:entering | return | endif
			let s:endCol = col - 1
		endif

		" If the cursor moves outside the snippet, quit it
		if line('.') != s:tab_stops[s:cur_stop][0] || col < s:startCol ||
					\ col - 1 > s:endCol
			unl! s:startCol s:origWordLen s:oldVars s:has_mirrors
			return s:RemoveSnippet()
		endif

		call s:UpdateVars()
		let s:prevLen = col('$')
	elseif exists('s:tab_stops')
		if !a:entering && s:tab_stops[s:cur_stop][2] != -1
			let s:tab_stops[s:cur_stop][2] = -2
		endif

		let col = col('.')
		let lnum = line('.')

		if lnum == s:endLine
			let s:endCol += col('$') - s:prevLen
			let s:prevLen = col('$')
		endif

		" Delete snippet if cursor moves out of it in insert mode
		if (lnum == s:endLine && (col > s:endCol || col < s:tab_stops[s:cur_stop][1]))
			\ || lnum > s:endLine || lnum < s:tab_stops[s:cur_stop][0]
			call s:RemoveSnippet()
		endif
	endif
endfunction

function! s:DeleteNestedPlaceholders()
	let s:nested_count = 0
	let lnum = line('.')
	let endPlaceholder = s:tab_stops[s:cur_stop][1] + s:tab_stops[s:cur_stop][2]
	let startPlaceholder = s:tab_stops[s:cur_stop][1]
	for tabstop in s:tab_stops[(s:cur_stop + 1):]
		if tabstop[0] != lnum ||
		 \ tabstop[1] > endPlaceholder || tabstop[1] < startPlaceholder
			break
		endif
		let s:nested_count += 1
	endfor
endfunction

" This updates the variables in a snippet when a placeholder has been edited.
" (e.g., each "$1" in "${1:foo} $1bar $1bar")
function! s:UpdateVars()
	let newWordLen = s:endCol - s:startCol + 1
	let newWord = strpart(getline('.'), s:startCol, newWordLen)
	if newWord == s:oldWord || empty(s:tab_stops[s:cur_stop][3])
		return
	endif

	let changeLen = s:tab_stops[s:cur_stop][2] - newWordLen
	let curLine = line('.')
	let startCol = col('.')
	let oldStartSnip = s:startCol
	let updateTabStops = changeLen != 0
	let i = 0

	for [lnum, col] in s:tab_stops[s:cur_stop][3]
		if updateTabStops
			let start = s:startCol
			if lnum == curLine && col <= start
				let s:startCol -= changeLen
				let s:endCol -= changeLen
			endif
			for nPos in s:tab_stops[s:cur_stop][3][(i):]
				" This list is in ascending order, so quit if we've gone too far.
				if nPos[0] > lnum | break | endif
				if nPos[0] == lnum && nPos[1] > col
					let nPos[1] -= changeLen
				endif
			endfor
			if lnum == curLine && col > start
				let col -= changeLen
				let s:tab_stops[s:cur_stop][3][i][1] = col
			endif
			let i += 1
		endif

		" "Very nomagic" is used here to allow special characters.
		call setline(lnum, substitute(getline(lnum), '\%'.col.'c\V'.
						\ escape(s:oldWord, '\'), escape(newWord, '\&'), ''))
	endfor
	if oldStartSnip != s:startCol
		call cursor(0, startCol + s:startCol - oldStartSnip)
	endif

	let s:oldWord = newWord
	let s:tab_stops[s:cur_stop][2] = newWordLen
endfunction
" vim:noet:sw=4:ts=4:ft=vim
" }}}

" restore 'cpo'
let &cpo = s:save_cpo

" vim:noet:sw=4:ts=4:ft=vim
