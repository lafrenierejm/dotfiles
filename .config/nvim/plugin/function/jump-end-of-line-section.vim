" Jump to the end of the current section within a line intelligently
" First go to the last non-whitescape column
" Then go to the last column

function! JumpEndOfLineSection()
	" Save the current column
	let current_col = virtcol('.')
	" Save the column number of the last non-whitespace character
	normal g_
	let significant_col = virtcol('.')
	" Go to the last column if current position is
	" at or after the last non-whitespace column
	if current_col >= significant_col
		normal $
	endif
endfunction
