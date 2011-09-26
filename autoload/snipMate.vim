" prototype dictionary copied to create buffer-local state variable
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
    " Update things just in case
    " AFAIK this is only needed because selecting items with the pop up menu
    " does not trigger CursorMovedI
    call self.update_changes()

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
