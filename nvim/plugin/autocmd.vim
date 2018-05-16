augroup BeforeWritingBuffer
	autocmd!
	autocmd BufWritePre * silent! :call StripHeadWhitespace()
	autocmd BufWritePre * silent! :call StripEndLine()
	autocmd BufWritePre * silent! :call NormalizeBlankLines()
augroup END

augroup AfterWritingBuffer
	autocmd!
	autocmd BufWritePost * silent! Neomake
augroup END
