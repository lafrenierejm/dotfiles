augroup BeforeWritingBuffer
	autocmd!
	autocmd BufWritePre * silent! :call StripHeadWhitespace()
	autocmd BufWritePre * silent! :call NormalizeBlankLines()
	autocmd BufWritePre * silent! :call StripEndWhitespace()
augroup END

augroup AfterWritingBuffer
	autocmd!
	autocmd BufWritePost * silent! Neomake
augroup END
