" Strip whitespace from the buffer's head

function! StripHeadWhitespace()
	let l:save_view=winsaveview()
	if !exists('b:keepHeadWhitespace')
		let l:pre_gdefault = &gdefault
		set nogdefault
		" Remove blank (whitespace only or empty) lines at head of file
		silent! :%s/\%^\(^\_s*\n\)\+//g
		" Strip whitespace at start of first line
		silent! :0,1s/\_s*\(\S\)/\1/
		if l:pre_gdefault
			set gdefault
		endif
	endif
	call winrestview(l:save_view)
endfunction
