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

inoremap <silent> <Plug>snipmateTrigger       <C-R>=<SID>TriggerSnippet()<CR>
inoremap <silent> <Plug>snipmateNext          <C-R>=<SID>NextTabStop()<CR>
inoremap <silent> <Plug>snipmatePrev          <C-R>=<SID>PrevTabStop()<CR>
inoremap <silent> <Plug>snipmateShow          <C-R>=<SID>ShowAvailableSnips()<CR>
snoremap <silent> <Plug>snipmateNext          <Esc>a<C-R>=<SID>NextTabStop()<CR>
snoremap <silent> <Plug>snipmatePrev          <Esc>a<C-R>=<SID>PrevTabStop()<CR>

function! s:map_if_not_mapped(lhs, rhs, mode)
    if !hasmapto(a:rhs, a:mode)
        silent! exe a:mode . 'map <unique>' a:lhs a:rhs
    endif
endfunction

call s:map_if_not_mapped('<Tab>',      '<Plug>snipmateTrigger', 'i')
call s:map_if_not_mapped('<S-Tab>',    '<Plug>snipmatePrev',    'i')
call s:map_if_not_mapped('<C-R><Tab>', '<Plug>snipmateShow',    'i')
call s:map_if_not_mapped('<Tab>',      '<Plug>snipmateNext',    's')
call s:map_if_not_mapped('<S-Tab>',    '<Plug>snipmatePrev',    's')

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
        for path in split(globpath(g:snippets_dir, ft . '.snippets'), "\n") +
                    \ split(globpath(g:snippets_dir, ft . '/*.snippets'), "\n")
            call s:ExtractSnipsFile(path, ft)
        endfor
        let s:did_ft[ft] = 1
    endfor
endfunction

" Parses a snippets file
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
    if exists('b:snipstate')
        let jump = s:NextTabStop()
        if type(jump) == 1 " returned a string
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
    endif
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
    call filter(snippets, 'v:val.word =~ "\\V\\^' . escape(a:trigger, '"\') . '"')
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
    return exists('b:snipstate') ? b:snipstate.jump_stop(1) : "\<s-tab>"
endfunction

function! s:NextTabStop()
    return exists('b:snipstate') ? b:snipstate.jump_stop(0) : ''
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

" restore 'cpo'
let &cpo = s:save_cpo

" vim:et:sw=4:
