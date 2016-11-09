" Attempt to run Neomake after every buffer write
function! AddNeomakePostWrite()
	" Test for the Neomake command
	if exists(':Neomake')
		augroup RunNeomakePostWrite
			autocmd!
			autocmd BufWritePost * Neomake
		augroup END
	endif
endfunction

" Run AddNeomakePostWrite() after loading any buffer
augroup TestNeomakePostWrite
	autocmd!
	autocmd VimEnter * call AddNeomakePostWrite()
augroup END
