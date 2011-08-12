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

au BufRead,BufNewFile *.snippets\= set ft=snippet
au FileType snippet setl noet fdm=expr fde=getline(v:lnum)!~'^\\t\\\\|^$'?'>1':1

" bind local dict to global dict (debugging purposes)
" you should use MakeSnip to add custom snippets
if !exists('g:multi_snips')
  let g:multi_snips = {}
endif
let s:multi_snips = g:multi_snips

if !exists('snippets_dir')
	let snippets_dir = substitute(globpath(&rtp, 'snippets/'), "\n", ',', 'g')
endif

fun! s:MakeSnip(scope, trigger, content)
	return s:MakeMultiSnip(a:scope, a:trigger, a:content, 'default')
endf

fun! s:MakeMultiSnip(scope, trigger, content, desc)
	if !has_key(s:multi_snips, a:scope)
		let s:multi_snips[a:scope] = {}
	endif
	if !has_key(s:multi_snips[a:scope], a:trigger)
		let s:multi_snips[a:scope][a:trigger] = [[a:desc, a:content]]
	else
    let s:multi_snips[a:scope][a:trigger] += [[a:desc, a:content]]
	endif
endf

fun! ExtractSnipsFile(file, ft)
	if !filereadable(a:file) | return | endif
	let text = readfile(a:file)
	let inSnip = 0
	for line in text + ["\n"]
		if inSnip && (line[0] == "\t" || line == '')
			let content .= strpart(line, 1)."\n"
			continue
		elseif inSnip
			if empty(desc)
				call s:MakeSnip(a:ft, trigger, content[:-2])
			else
				call s:MakeMultiSnip(a:ft, trigger, content[:-2], desc)
			endif
			let inSnip = 0
		endif

		if line[:6] == 'snippet'
			let inSnip = 1
			let trigger = strpart(line, 8)
			let desc = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process multi snip
				let desc = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''
		endif
	endfor
endf

" Reset snippets for filetype.
fun! ResetSnippets(ft)
	let ft = a:ft == '' ? '_' : a:ft
	for dict in [s:multi_snips, g:did_ft]
		if has_key(dict, ft)
			unlet dict[ft]
		endif
	endfor
endf

" Reset snippets for all filetypes.
fun! ResetAllSnippets()
	let s:multi_snips = {} | let g:did_ft = {}
endf

" Reload snippets for filetype.
fun! ReloadSnippets(ft)
	let ft = a:ft == '' ? '_' : a:ft
	call ResetSnippets(ft)
	call CreateSnippets(g:snippets_dir, ft)
endf

" Reload snippets for all filetypes.
fun! ReloadAllSnippets()
	for ft in keys(g:did_ft)
		call ReloadSnippets(ft)
	endfor
endf

let g:did_ft = {}
fun! CreateSnippets(dir, filetypes)
	for ft in split(a:filetypes, '\.')
		if has_key(g:did_ft, ft) | continue | endif
		call s:DefineSnips(a:dir, ft, ft)
		if ft == 'objc' || ft == 'cpp' || ft == 'cs'
			call s:DefineSnips(a:dir, 'c', ft)
		elseif ft == 'xhtml'
			call s:DefineSnips(a:dir, 'html', 'xhtml')
		endif
		let g:did_ft[ft] = 1
	endfor
endf

" Define "aliasft" snippets for the filetype "realft".
fun s:DefineSnips(dir, aliasft, realft)
	for path in split(globpath(a:dir, a:aliasft.'.snippets')."\n".globpath(a:dir, a:aliasft.'-*.snippets'), "\n")
		call ExtractSnipsFile(path, a:realft)
	endfor
endf

fun! TriggerSnippet()
	if exists('g:SuperTabMappingForward')
		if g:SuperTabMappingForward == "<tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabForward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-n>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		elseif g:SuperTabMappingBackward == "<tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabBackward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-p>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		endif
	endif

	if pumvisible() " Update snippet if completion is used, or deal with supertab
		if exists('SuperTabKey')
			call feedkeys(SuperTabKey) | return ''
		endif
		call feedkeys("\<esc>a", 'n') " Close completion menu
		call feedkeys("\<tab>") | return ''
	endif

	if exists('g:snipPos') | return snipMate#jumpTabStop(0) | endif

	let word = matchstr(getline('.'), '\S\+\%'.col('.').'c')
	let multisnip = 0
	for scope in [bufnr('%')] + split(&ft, '\.') + ['_']
		try
			let [trigger, snippet] = s:GetSnippet(word, scope)
		catch /^snipMate: multisnip/
			let multisnip = 1
		endtry
		" If word is a trigger for a snippet, delete the trigger & expand
		" the snippet.
		if snippet != '' && !multisnip
			let col = col('.') - len(trigger)
			sil exe 's/\V'.escape(trigger, '/\.').'\%#//'
			return snipMate#expandSnip(snippet, col)
		endif
	endfor

	" launch CompleteSnippets() for multi snips
	if multisnip == 1
		return CompleteSnippets()
	endif

	if exists('SuperTabKey')
		call feedkeys(SuperTabKey)
		return ''
	endif
	return "\<tab>"
endf

fun! CompleteSnippets()
	let line = getline('.')
	let cur = col('.') - 1
	let start = cur

	" find completion starting position
	while start > 0
		if line[start - 1] =~ '\S'
			let start -= 1
		else
			break
		endif
	endwhile

	let word = strpart(line, start, (cur-start))

	" get possible snippets
	let snippets = []
	for scope in [bufnr('%')] + split(&ft, '\.') + ['_']
		if has_key(s:multi_snips, scope)
			for key in keys(s:multi_snips[scope])
				let i = 1
				for description in s:multi_snips[scope][key]
					let item = {}
					let item['word'] = key . '_' . i
					let item['menu'] = description[0]
					let item['dup'] = '1'
					call insert(snippets, item)
					let i += 1
				endfor
			endfor
		endif
	endfor
	call filter(snippets, 'v:val.word =~ "^'.word.'"')
	call sort(snippets)
	call complete(start+1, snippets)
	if len(snippets) == 1
		return "\<c-r>=TriggerSnippet()\<cr>"
	endif
	return ''
endf

fun! BackwardsSnippet()
	if exists('g:snipPos') | return snipMate#jumpTabStop(1) | endif

	if exists('g:SuperTabMappingForward')
		if g:SuperTabMappingForward == "<s-tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabForward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-n>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		elseif g:SuperTabMappingBackward == "<s-tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabBackward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-p>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		endif
	endif
	if exists('SuperTabKey')
		call feedkeys(SuperTabKey)
		return ''
	endif
	return "\<s-tab>"
endf

" Check if word under cursor is snippet trigger; if it isn't, try checking if
" the text after non-word characters is (e.g. check for "foo" in "bar.foo")
fun s:GetSnippet(word, scope)
	let word = a:word | let snippet = ''
	while snippet == ''
		if exists('s:multi_snips["'.a:scope.'"]["'.escape(word, '\"').'"]')
			throw 'snipMate: multisnip'
		elseif match(word, '_\d\+$') != -1
			let id = matchstr(word, '_\zs\d\+$') - 1
			let snip = matchstr(word, '^.*\ze_\d\+$')
			if exists('s:multi_snips["'.a:scope.'"]["'.escape(snip, '\"').'"]')
				let snippet = s:multi_snips[a:scope][snip][id][1]
				if snippet == '' | break | endif
			else
				break
			endif
		else
			if match(word, '\W') == -1 | break | endif
			let word = substitute(word, '.\{-}\W', '', '')
		endif
	endw
	if word == '' && a:word != '.' && stridx(a:word, '.') != -1
		let [word, snippet] = s:GetSnippet('.', a:scope)
	endif
	return [word, snippet]
endf

fun! ShowAvailableSnips()
	let line  = getline('.')
	let col   = col('.')
	let word  = matchstr(getline('.'), '\S\+\%'.col.'c')
	let words = [word]
	if stridx(word, '.')
		let words += split(word, '\.', 1)
	endif
	let matchlen = 0
	let matches = []
	for scope in [bufnr('%')] + split(&ft, '\.') + ['_']
		for trigger in keys(get(s:multi_snips, scope, {}))
			for word in words
				if word == ''
					let matches += [trigger] " Show all matches if word is empty
				elseif trigger =~ '^'.word
					let matches += [trigger]
					let len = len(word)
					if len > matchlen | let matchlen = len | endif
				endif
			endfor
		endfor
	endfor

	" This is to avoid a bug with Vim when using complete(col - matchlen, matches)
	" (Issue#46 on the Google Code snipMate issue tracker).
	call setline(line('.'), substitute(line, repeat('.', matchlen).'\%'.col.'c', '', ''))
	call complete(col, matches)
	return ''
endf
" http://www.vim.org/scripts/script.php?script_id=1879
" To enable auto-popup for this completion, add following function to plugin/snipMate.vim:
fun! GetSnipsInCurrentScope()
  let snips = {}
  for scope in [bufnr('%')] + split(&ft, '\.') + ['_']
    call extend(snips, get(s:multi_snips, scope, {}), 'keep')
  endfor
  return snips
endf
" vim:noet:sw=4:ts=4:ft=vim
