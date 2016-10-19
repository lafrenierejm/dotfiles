" Insert an empty line above the current line
function! InsertEmptyLineAbove()
	let l:save_view=winsaveview()
	" `put!` inserts text in register above current line
	" '_' is the black hole register
	put!_
	call winrestview(l:save_view)
endfunction

" Insert an empty line below the current line
function! InsertEmptyLineBelow()
	let l:save_view=winsaveview()
	" `put` inserts text in register after current line
	" '_' is the black hole register
	put_
	call winrestview(l:save_view)
endfunction
