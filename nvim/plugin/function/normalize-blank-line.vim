" Normalize the number of consecutive blank lines.
function! NormalizeBlankLines()
	let l:save_view=winsaveview()
	if exists('b:maxConsecutiveEmptyLines')
		if b:maxConsecutiveEmptyLines >= 1
			let l:newLines=''
			let l:numLines=0
			" exceed desired lines by one to accommodate for the wanted line matched
			while l:numLines <=  b:maxConsecutiveEmptyLines
				let l:newLines .= '\r'
				let l:numLines += 1
			endwhile
			" exceed desired lines by one to accommodate for the wanted line matched
			let l:numLines=b:maxConsecutiveEmptyLines + 1
			silent execute ':%s/\n\{' . l:numLines . ',}/' . l:newLines . '/e'
		endif
	else
		silent execute ':%s/\n\{2,}/\r\r/e'
	endif
	call winrestview(l:save_view)
endfunction
