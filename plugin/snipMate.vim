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
    call filter(snippets, 'v:val.word =~ "\\V^' . escape(a:trigger, '"\') . '"')
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
" autoload {{{

let s:state_proto = {}

" Removes snippet state info
function! s:state_proto.remove()
    " Remove all autocmds in group snipmate_changes in the current buffer
    au! snipmate_changes * <buffer>
    unl! b:snipstate
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

    let b:snipstate = copy(s:state_proto)

    " store tab stop locations and the number of them
    let [b:snipstate.stops, b:snipstate.stop_count] = s:BuildTabStops(snippet, lnum, col - indent, indent)

    if b:snipstate.stop_count
        " Update the snippet when entering insert mode and when the cursor moves
        aug snipmate_changes
            au CursorMovedI <buffer> call b:snipstate.update_changes()
            au InsertEnter <buffer> call b:snipstate.update_changes()
        aug END
        call b:snipstate.set_stop(0)

        return b:snipstate.select_word()
    else
        unl b:snipstate
        " Place cursor at end of snippet if no tab stop is given
        let newlines = len(snipLines) - 1
        call cursor(lnum + newlines, indent + len(snipLines[-1]) - len(afterCursor)
                    \ + (newlines ? 0: col - 1))
    endif
    return ''
endfunction

" Update state information to correspond to the given tab stop
function! s:state_proto.set_stop(stop)
    let self.stop_no     = a:stop
    let self.cur_stop    = self.stops[self.stop_no]
    let self.endCol      = self.cur_stop[1] + self.cur_stop[2]
    let self.startCol    = self.cur_stop[1]
    let self.has_mirrors = !empty(self.cur_stop[3])
    call cursor(self.cur_stop[0], self.cur_stop[1])
    let self.prevLen = col('$')
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
    " (e.g. "${1:foo}" turns all "$1"'s into "foo$1")
    let i = 1
    while stridx(snippet, '${'.i) != -1
        let s = s:GetPlaceholder(snippet, i)
        if s != ''
            " Take care of nested placeholders by removing the surrounding ${:}
            let s = s:RemoveTabStops(s)
            let snippet = substitute(snippet, '$'.i, s.'&', 'g')
        endif
        let i += 1
    endwhile

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
    endwhile
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
    endwhile
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
    endwhile
endfunction

" Counts occurences of needle in haystack
function! s:Count(haystack, needle)
    let counter = 0
    let index = stridx(a:haystack, a:needle)
    while index != -1
        let index = stridx(a:haystack, a:needle, index+1)
        let counter += 1
    endwhile
    return counter
endfunction

" Builds a list of a list of each tab stop in the snippet containing:
" 1.) The tab stop's line number.
" 2.) The tab stop's column number
"     (by getting the length of the string between the last "\n" and the
"     tab stop).
" 3.) The length of the placeholder for the current tab stop or 0 for no
"     placeholder. "${1}" would be 0; "${1:foo}" would be 3.
" 4.) A list of [line, column] representing any mirrors for the tab stop. The
"     list is empty if there are no mirrors.
function! s:BuildTabStops(snip, lnum, col, indent)
    let snipPos = []
    let i = 1
    let withoutVars = substitute(a:snip, '$\d\+', '', 'g')
    let noTabStops = s:RemoveTabStops(a:snip)
    while stridx(a:snip, '${'.i) != -1
        let beforeTabStop = matchstr(withoutVars, '^.*\ze${'.i.'\D')
        let withoutOthers = s:RemoveAllExcept(withoutVars, i)

        let j = i - 1
        call add(snipPos, [0, 0, 0])
        let snipPos[j][0] = a:lnum + s:Count(beforeTabStop, "\n")
        let snipPos[j][1] = a:indent + len(matchstr(withoutOthers, '.*\(\n\|^\)\zs.*\ze${'.i))
        if snipPos[j][0] == a:lnum | let snipPos[j][1] += a:col | endif

        " Get all $# matches in another list, if ${#:name} is given
        " if stridx(withoutVars, '${'.i.':') != -1
            let snipPos[j][2] = strwidth(s:RemoveTabStops(s:GetPlaceholder(withoutVars, i)))
            let dots = repeat('.', snipPos[j][2])
            call add(snipPos[j], [])

            " remove mirrors for other tab stops
            let withoutOthers = substitute(noTabStops, '$'.i.'\@!\d\+', '', 'g')
            while match(withoutOthers, '$'.i.'\(\D\|$\)') != -1
                let beforeMark = matchstr(withoutOthers, '^.\{-}\ze'.dots.'$'.i.'\(\D\|$\)')
                call add(snipPos[j][3], [0, 0])
                let snipPos[j][3][-1][0] = a:lnum + s:Count(beforeMark, "\n")
                let snipPos[j][3][-1][1] = a:indent + (snipPos[j][3][-1][0] > a:lnum
                                           \ ? len(matchstr(beforeMark, '.*\n\zs.*'))
                                           \ : a:col + len(beforeMark))
                let withoutOthers = substitute(withoutOthers, '$'.i.'\ze\(\D\|$\)', '', '')
            endwhile
        " endif
        let i += 1
    endwhile
    return [snipPos, i - 1]
endfunction

" Jump to the next/previous tab stop
function! s:state_proto.jump_stop(backwards)
    " Update the locations of tab stops for any changes made
    if self.has_mirrors
        call self.update_placeholders()
    else
        call self.update_stops()
    endif

    let self.cur_stop[1] = self.startCol
    let self.cur_stop[2] = self.endCol - self.startCol

    let self.stop_no += a:backwards ? -1 : 1
    " Loop over the snippet when going backwards from the beginning
    if self.stop_no < 0 | let self.stop_no = self.stop_count - 1 | endif

    if exists('self.nested_count') " If a nested placeholder has been added, skip past it.
        let self.stop_no += self.nested_count
        unl self.nested_count
    endif
    if self.stop_no == self.stop_count
        call self.remove()
        return -1
    endif

    call self.set_stop(self.stop_no)
    return self.select_word()
endfunction

" Updates tab stops/mirrors
function! s:state_proto.update_placeholders()
    let changeLen = self.endCol - self.cur_stop[2] - self.startCol
    if !exists('self.oldVars') | return | endif
    " Update tab stops in snippet if text has been added via "$#"
    " (e.g., in "${1:foo}bar$1${2}").
    if changeLen != 0
        let curLine = line('.')

        for pos in self.stops
            if pos == self.cur_stop | continue | endif
            let changed = pos[0] == curLine && pos[1] > self.startCol
            let changedVars = 0
            let endPlaceholder = pos[2] - 1 + pos[1]
            " Subtract changeLen from each tab stop that was after any of
            " the current tab stop's placeholders.
            for [lnum, col] in self.oldVars
                if lnum > pos[0] | break | endif
                if pos[0] == lnum
                    if pos[1] > col || (pos[2] == -1 && pos[1] == col)
                        let changed += 1
                    elseif col < endPlaceholder
                        let changedVars += 1
                    endif
                endif
            endfor
            let pos[1] += changeLen * changed
            let pos[2] += changeLen * changedVars " Parse variables within placeholders
                                                  " e.g., "${1:foo} ${2:$1bar}"

            " Do the same to any placeholders in the other tab stops.
            for nPos in pos[3]
                let changed = nPos[0] == curLine && nPos[1] > self.startCol
                for [lnum, col] in self.oldVars
                    if lnum > nPos[0] | break | endif
                    if nPos[0] == lnum && nPos[1] > col
                        let changed += 1
                    endif
                endfor
                let nPos[1] += changeLen * changed
            endfor
        endfor
    endif
    unl self.oldVars
endfunction

" Updates tab stops/mirrors
function! s:state_proto.update_stops()
    let changeCol = self.endCol - self.cur_stop[2] - self.startCol
    let lnum = self.cur_stop[0]
    let col = self.cur_stop[1]

    if changeCol != 0
        " Update the column of all proceeding tab stops if text has
        " been inserted/deleted in the current line.
        for pos in self.stops
            if pos[1] >= col && pos[0] == lnum
                let pos[1] += changeCol
            endif
            for nPos in pos[3]
                if nPos[0] > lnum | break | endif
                if nPos[0] == lnum && nPos[1] >= col
                    let nPos[1] += changeCol
                endif
            endfor
        endfor
    endif
endfunction

" Select the placeholder for the current tab stop
function! s:state_proto.select_word()
    let len = self.cur_stop[2]
    if !len | return '' | endif
    let l = col('.') != 1 ? 'l' : ''
    if &sel == 'exclusive'
        return "\<esc>".l.'v'.len."l\<c-g>"
    endif
    return len == 1 ? "\<esc>".l.'gh' : "\<esc>".l.'v'.(len - 1)."l\<c-g>"
endfunction

" Update the snippet as text is typed. The self.update_mirrors() function does
" the actual work.
" If the cursor moves outside of a placeholder, call self.remove()
function! s:state_proto.update_changes()
    " If tab stop has been modified, delete any nested placeholders it has.
    if !exists('self.nested_count') &&
                \ col('$') - (self.prevLen + self.cur_stop[2])
        call self.delete_nested()
    endif

    let changeLen = col('$') - self.prevLen
    let self.endCol += changeLen

    let col = col('.')
    if line('.') != self.cur_stop[0] || col < self.startCol || col > self.endCol
        call self.remove()
    endif

    if self.has_mirrors
        if !exists('self.oldVars') && self.stop_no + 1 < self.stop_count
            " Save the old snippet & word length before it's updated.
            " self.startCol must be saved too, in case text is added
            " before the snippet (e.g. in "foo$1${2}bar${1:foo}").
            let self.oldVars = deepcopy(self.cur_stop[3])
        endif

        call self.update_mirrors(changeLen)
    endif

    let self.prevLen = col('$')
endfunction

" Delete any nested tab stops
function! s:state_proto.delete_nested()
    let self.nested_count = 0
    let lnum = line('.')
    let endPlaceholder = self.cur_stop[1] + self.cur_stop[2]
    let startPlaceholder = self.cur_stop[1]
    for tabstop in self.stops[(self.stop_no + 1):]
        if tabstop[0] != lnum ||
         \ tabstop[1] > endPlaceholder || tabstop[1] < startPlaceholder
            break
        endif
        let self.nested_count += 1
    endfor
endfunction

" Actually update the mirrors for any changed text
function! s:state_proto.update_mirrors(change)
    let newWordLen = self.endCol - self.startCol
    let newWord = strpart(getline('.'), self.startCol - 1, newWordLen)
    let changeLen = a:change
    let curLine = line('.')
    let startCol = col('.')
    let oldStartSnip = self.startCol
    let updateTabStops = changeLen != 0
    let i = 0

    for [lnum, col] in self.cur_stop[3]
        if updateTabStops
            let start = self.startCol
            if lnum == curLine && col <= start
                let self.startCol += changeLen
                let self.endCol += changeLen
            endif
            for nPos in self.cur_stop[3][(i):]
                " This list is in ascending order, so quit if we've gone too far.
                if nPos[0] > lnum | break | endif
                if nPos[0] == lnum && nPos[1] > col
                    let nPos[1] += changeLen
                endif
            endfor
            if lnum == curLine && col > start
                let col += changeLen
                let self.cur_stop[3][i][1] = col
            endif
            let i += 1
        endif

        let theline = getline(lnum)
        " subtract -1 to go from column byte index to string byte index
        " subtract another -1 to exclude the col'th element
        call setline(lnum, theline[0:(col-2)] . newWord . theline[(col+self.endCol-self.startCol-a:change-1):])
    endfor
endfunction
" }}}

" restore 'cpo'
let &cpo = s:save_cpo

" vim:et:sw=4:
