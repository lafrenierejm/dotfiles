" Jump to the beginning intelligently
" First go to the first non-whitespace column
" Then go to the first column

function! JumpStartOfLineSection()
	" Get the column number of the current character
	let current_col=virtcol('.')
	" Go to the column number of the first non-whitespace character
	normal ^
	let significant_col=virtcol('.')
	" Go to the first column if current position is
	" at or before first non-whitespace column
	if current_col<=significant_col
		normal 0
	endif
endfunction
