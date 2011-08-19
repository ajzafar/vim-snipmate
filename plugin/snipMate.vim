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

augroup snipmate
	au BufRead,BufNewFile *.snippets\= set ft=snippet
	au FileType snippet setl noet fdm=expr fde=getline(v:lnum)!~'^\\t\\\\|^$'?'>1':1
	au VimEnter * call CreateSnippets(snippets_dir, '_') " Get global snippets
	au FileType * if &ma | call CreateSnippets(snippets_dir, &ft) | endif
augroup END

inoremap <silent> <Plug>snipmateTrigger  <C-R>=TriggerSnippet()<CR>
inoremap <silent> <Plug>snipmateBack     <C-R>=BackwardsSnippet()<CR>
inoremap <silent> <Plug>snipmateShow     <C-R>=ShowAvailableSnips()<CR>
smap     <silent> <Plug>ssnipmateTrigger <Esc>a<Plug>snipmateTrigger
smap     <silent> <Plug>ssnipmateBack    <Esc>a<Plug>snipmateBack

imap <Tab>      <Plug>snipmateTrigger
imap <S-Tab>    <Plug>snipmateBack
imap <C-R><Tab> <Plug>snipmateShow
smap <Tab>      <Plug>ssnipmateTrigger
smap <S-Tab>    <Plug>ssnipmateBack

let s:multi_snips = {}
let g:multi_snips = s:multi_snips

if !exists('snippets_dir')
	let snippets_dir = substitute(globpath(&rtp, 'snippets/'), "\n", ',', 'g')
endif

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
	let valid = 1
	for line in text + ["\n"]
		if inSnip
			if (line[0] == "\t" || line == '')
				let content .= strpart(line, 1)."\n"
				continue
			elseif valid
				call s:MakeMultiSnip(a:ft, trigger, content[:-2], desc)
			endif
			let inSnip = 0
		endif

		if line[:6] == 'snippet'
			let inSnip = 1
			let valid = 1
			let trigger = strpart(line, 8)
			let desc = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process multi snip
				let desc = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''

			if trigger !~ s:GetTriggerRegex('$', '^')
				echom 'Invalid snippet trigger' trigger
				let valid = 0
			endif
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
	for path in [expand(a:dir).'/'.a:aliasft.'.snippets'] + split(globpath(a:dir, a:aliasft.'/*.snippets'), "\n")
		call ExtractSnipsFile(path, a:realft)
	endfor
endf

fun! TriggerSnippet()
	if exists('g:snipPos') | return snipMate#jumpTabStop(0) | endif

	" Grab the trigger (and where it begins)
	let [trigger, begin] = s:GrabTrigger()
	" See if we have a snippet for that
	for scope in split(&ft, '\.') + ['_']
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
		return TriggerSnippet()
	else
	" return nothing otherwise we break the completion
		return ''
	end
endf

fun! s:GetTriggerRegex(end, ...)
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
endf

fun! s:GrabTrigger()
	let t = matchstr(getline('.'), s:GetTriggerRegex('\%' . col('.') . 'c'))
	return [t, col('.') - len(t)]
endf

fun! s:GetMatches(trigger)

	" get possible snippets
	let snippets = []
	for scope in split(&ft, '\.') + ['_']
		for key in keys(get(s:multi_snips, scope, {}))
			let i = 1
			for description in s:multi_snips[scope][key]
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
	call filter(snippets, 'v:val.word =~ "^'.a:trigger.'"')
	call sort(snippets)
	return snippets
endf

fun! BackwardsSnippet()
	if exists('g:snipPos') | return snipMate#jumpTabStop(1) | endif

	return "\<s-tab>"
endf

" Check if trigger is the only usable trigger
fun s:GetSnippet(trigger, scope)
	let snippets = get(s:multi_snips[a:scope], a:trigger, [])
	let id = matchstr(a:trigger, '_\zs\d\+$')
	if id != ''
		let snip = matchstr(a:trigger, '^.*\ze_\d\+$')
		let snippets = get(s:multi_snips[a:scope], snip, [])
	endif
	return len(snippets) == 1 ? snippets[id - 1][1] : ''
endf

fun! ShowAvailableSnips()
	let [trigger, begin] = s:GrabTrigger()
	let matches = s:GetMatches(trigger)
	call complete(begin, matches)
	return ''
endf

" vim:noet:sw=4:ts=4:ft=vim
