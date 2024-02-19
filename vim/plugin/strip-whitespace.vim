" Strip whitespace from the buffer's head
function! StripHeadWhitespace()
	let l:save_view=winsaveview()
	if !exists('b:keepHeadWhitespace')
		" Remove blank (whitespace only or empty) lines at head of file
		silent! :%s/\%^\(^\_s*\n\)\+//g
		" Strip whitespace at start of first line
		silent :0,1s/\_s*\(\S\)/\1/
	endif
	call winrestview(l:save_view)
endfunction

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

" Normalize consecutive blank lines
function! NormalizeBlankLines()
	let l:save_view=winsaveview()
	if exists('b:maxConsecutiveEmptyLines')
		let l:newLines=''
		let l:numLines=0
		" Exceed desired lines by one to accommodate for the wanted line matched
		while l:numLines <=  b:maxConsecutiveEmptyLines
			let l:newLines .= '\r'
			let l:numLines += 1
		endwhile
		" Exceed desired lines by one to accommodate for the wanted line matched
		let l:numLines=b:maxConsecutiveEmptyLines + 1
		silent execute ':%s/\n\{' . l:numLines . ',}/' . l:newLines . '/e'
	else
		silent :%s/\n\{2,}/\r\r/e
	endif
	call winrestview(l:save_view)
endfunction

" Define an autocmd group for running the whitespace functions
augroup strip-whitespace
	autocmd!
	autocmd BufWritePre * :call StripHeadWhitespace()
	autocmd BufWritePre * :call StripTrailWhitespace()
	autocmd BufWritePre * :call NormalizeBlankLines()
augroup END
