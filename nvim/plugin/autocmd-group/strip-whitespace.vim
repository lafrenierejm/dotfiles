augroup StripWhitespace
	" Clear the augroup
	autocmd!

	" Remove whitespace at buffer heads
	autocmd BufWritePre * :call StripHeadWhitespace()

	" Remove whitespace at ends of lines and buffers
	autocmd BufWritePre * :call StripTrailWhitespace()

	" Normalize the number of consecutive blank lines
	autocmd BufWritePre * :call NormalizeBlankLines()
augroup END
