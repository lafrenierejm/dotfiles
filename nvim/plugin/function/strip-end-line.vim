" Delete lines matching a regex from the end of the buffer.

" Author: Joseph LaFreniere <joseph@lafreniere.xyz>
" License: ISC License (https://opensource.org/licenses/isc-license.txt)

" If b:keep_end_lines is defined then nothing will be deleted.
" By default lines that are blank (empty or whitespace-only) will be removed.
" If `b:strip_end_regex` is defined then its value is instead used to match
" the lines to be removed.

function! StripEndLine()
	if !exists('b:keep_end_lines')
		let l:save_view=winsaveview()

		if exists('b:strip_end_regex')
			let l:regex = b:strip_end_regex
		else
			let l:regex = '\s*'
		endif

		execute '%s/\(\_$\n' . l:regex . '\)\+\%$//e'

		call winrestview(l:save_view)
	endif

	return
endfunction
