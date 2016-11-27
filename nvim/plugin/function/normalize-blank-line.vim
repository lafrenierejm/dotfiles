" Normalize consecutive blank lines

function! NormalizeBlankLines()
	let l:save_view=winsaveview()
	if exists('b:maxConsecutiveEmptyLines')
		let l:newLines=''
		let l:numLines=0
		" Exceed desired lines by one to accomodate for the wanted line matched
		while l:numLines <=  b:maxConsecutiveEmptyLines
			let l:newLines .= '\r'
			let l:numLines += 1
		endwhile
		" Exceed desired lines by one to accomodate for the wanted line matched
		let l:numLines=b:maxConsecutiveEmptyLines + 1
		silent execute ':%s/\n\{' . l:numLines . ',}/' . l:newLines . '/e'
	else
		silent :%s/\n\{2,}/\r\r/e
	endif
	call winrestview(l:save_view)
endfunction