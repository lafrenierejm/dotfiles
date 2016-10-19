" Strip blank email quote lines at end of buffer

function! StripTrailMailQuote()
	let l:save_view=winsaveview()
	" Remove blank lines at end of file
	silent :vglobal/\_s*\S/d
	" Remove lines containing only '>' and whitespace
	"" '\_$\n' is a newline (end of a line followed by return)
	"" '\s*' is zero or more whitespace characters
	"" '>\+' is one or more '>'
	"" '\s*' is zero or more whitespace characters
	"" '\%$' is the end of the file
	silent! :%s/\(\_$\n\s*>\+\s*\)\+\%$
	" Remove blank lines at end of file
	silent :vglobal/\_s*\S/d
	call winrestview(l:save_view)
endfunction
