" Strip all blank (empty or whitespace-only) lines at the end of the buffer.
" All credit for the commands actually performing the replacement goes to
" Benoit (https://stackoverflow.com/a/7496085).

scriptencoding utf8

function! StripEndWhitespace()
	let l:save_view=winsaveview()

	" Go to last line of file.
	:$
	" Insert an empty line below current line.
	put _
	" Starting at the last line in file `$;`,
	" search backwards `?`
	" for a line that is not completely blank `\(^\s*$\)\@!`.
	" `×××+1,$` forms a range from line `×××+1` till the last line.
	" The delete `d` operator is applied to that range.
	$;?\(^\s*$\)\@!?+1,$d

	call winrestview(l:save_view)
endfunction
