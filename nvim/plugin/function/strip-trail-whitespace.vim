" Strip trailing whitespace
function! StripTrailWhitespace()
	let l:save_view=winsaveview()
	if !exists('b:keepLineTrailWhitespace')
		" Remove non-newline whitespace at end of lines
		silent :%s/\s\+$//e
	endif
	if !exists('b:keepFileTrailWhitespace')
		" Remove blank (whitespace only or empty) lines at end of file
		silent :vglobal/\_s*\S/d
	endif
	call winrestview(l:save_view)
endfunction
