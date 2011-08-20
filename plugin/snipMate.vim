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

augroup snipmate
	au BufRead,BufNewFile *.snippets\= set ft=snippet
	au FileType snippet setl noet fdm=expr fde=getline(v:lnum)!~'^\\t\\\\|^$'?'>1':1
	au VimEnter * call s:CreateSnippets(snippets_dir, ['_'])
	au FileType * if &ma | call s:CreateSnippets(snippets_dir, split(&ft, '\.')) | endif
augroup END

inoremap <silent> <Plug>snipmateTrigger  <C-R>=<SID>TriggerSnippet()<CR>
inoremap <silent> <Plug>snipmateBack     <C-R>=<SID>BackwardsSnippet()<CR>
inoremap <silent> <Plug>snipmateShow     <C-R>=<SID>ShowAvailableSnips()<CR>
smap     <silent> <Plug>ssnipmateTrigger <Esc>a<Plug>snipmateTrigger
smap     <silent> <Plug>ssnipmateBack    <Esc>a<Plug>snipmateBack

imap <Tab>      <Plug>snipmateTrigger
imap <S-Tab>    <Plug>snipmateBack
imap <C-R><Tab> <Plug>snipmateShow
smap <Tab>      <Plug>ssnipmateTrigger
smap <S-Tab>    <Plug>ssnipmateBack

command! -complete=filetype -nargs=* -bar
			\ ReloadSnippets call s:ReloadSnippets(<f-args>)
command! -complete=filetype -nargs=* -bar
			\ ResetSnippets call s:ResetSnippets(<f-args>)

let s:multi_snips = {}

if !exists('snippets_dir')
	let snippets_dir = substitute(globpath(&rtp, 'snippets/'), "\n", ',', 'g')
endif

function! s:MakeMultiSnip(scope, trigger, content, desc)
	if !has_key(s:multi_snips, a:scope)
		let s:multi_snips[a:scope] = {}
	endif
	if !has_key(s:multi_snips[a:scope], a:trigger)
		let s:multi_snips[a:scope][a:trigger] = [[a:desc, a:content]]
	else
		let s:multi_snips[a:scope][a:trigger] += [[a:desc, a:content]]
	endif
endfunction

function! s:ExtractSnipsFile(file, ft)
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
endfunction

let s:did_ft = {}
function! s:CreateSnippets(dir, ...)
	let scopes = a:0 ? a:1 : s:GetScopes()
	for ft in scopes
		if has_key(s:did_ft, ft) | continue | endif
		call s:DefineSnips(a:dir, ft, ft)
		if ft == 'objc' || ft == 'cpp' || ft == 'cs'
			call s:DefineSnips(a:dir, 'c', ft)
		elseif ft == 'xhtml'
			call s:DefineSnips(a:dir, 'html', 'xhtml')
		endif
		let s:did_ft[ft] = 1
	endfor
endfunction

" Define "aliasft" snippets for the filetype "realft".
function! s:DefineSnips(dir, aliasft, realft)
	for path in [expand(a:dir).'/'.a:aliasft.'.snippets'] + split(globpath(a:dir, a:aliasft.'/*.snippets'), "\n")
		call s:ExtractSnipsFile(path, a:realft)
	endfor
endfunction

function! s:TriggerSnippet()
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
		return s:TriggerSnippet()
	else
	" return nothing otherwise we break the completion
		return ''
	end
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

function! s:GrabTrigger()
	let t = matchstr(getline('.'), s:GetTriggerRegex('\%' . col('.') . 'c'))
	return [t, col('.') - len(t)]
endfunction

function! s:GetMatches(trigger)

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
endfunction

function! s:BackwardsSnippet()
	if exists('g:snipPos') | return snipMate#jumpTabStop(1) | endif

	return "\<s-tab>"
endfunction

" Check if trigger is the only usable trigger
function! s:GetSnippet(trigger, scope)
	let snippets = get(s:multi_snips[a:scope], a:trigger, [])
	let id = matchstr(a:trigger, '_\zs\d\+$')
	if id != ''
		let snip = matchstr(a:trigger, '^.*\ze_\d\+$')
		let snippets = get(s:multi_snips[a:scope], snip, [])
	endif
	return len(snippets) == 1 ? snippets[id - 1][1] : ''
endfunction

function! s:ShowAvailableSnips()
	let [trigger, begin] = s:GrabTrigger()
	let matches = s:GetMatches(trigger)
	call complete(begin, matches)
	return ''
endfunction

function! s:GetScopes()
	return split(&ft, '\.') + ['_']
endfunction

" Reload snippets for filetype.
function! s:ReloadSnippets(...)
	let scopes = a:0 ? a:000 : s:GetScopes()
	call call('s:ResetSnippets', scopes)
	call s:CreateSnippets(g:snippets_dir, scopes)
endfunction

" Reset snippets for filetype.
function! s:ResetSnippets(...)
	let scopes = a:0 ? a:000 : s:GetScopes()
	for dict in [s:multi_snips, s:did_ft]
		for scope in scopes
			unlet! dict[scope]
		endfor
	endfor
endfunction

" restore 'cpo'
let &cpo = s:save_cpo

" vim:noet:sw=4:ts=4:ft=vim
