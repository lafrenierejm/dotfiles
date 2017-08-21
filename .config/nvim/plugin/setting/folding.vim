" Folding
if has('folding')
	set foldenable " Enable folding
	set foldnestmax=10 " Limit maximum embedded folds to 10
	set foldmethod=expr
	set foldexpr=FoldMethod(v:lnum)
endif

" Credit to tinmarino (https://vi.stackexchange.com/a/5247)
function! FoldMethod(lnum)
	" Get string of current line
	let l:cur_line=getline(a:lnum)

	" Check if empty line
	if empty(l:cur_line) " Empty line or end comment
		return -1 " So same indent level as line before
	endif

	" Check if comment
	let a:data=join(map(synstack(a:lnum, 1), 'synIDattr(v:val, ''name'')') )
	if a:data =~ '.*omment.*'
		return '='

	" Otherwise return foldlevel equal to indent/shiftwidth
	" (like if foldmethod=indent)
	else  " return indent base fold
		return indent(a:lnum)/&shiftwidth
	endif
endfunction
