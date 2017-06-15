" Return a movement string to go to either the first glyph or, if the cursor
" is already at or before that glyph, the beginning of the line.
" If `wrap` is enabled on the cursor is on a visual line not actually present
" in the underlying file, the returned string will move the cursor to the
" beginning of the visual line.
function! SmartHome()
	" Assume that the cursor should go to the first glyph.
	let l:movement_string='^'

	" Get the column number of the first glyph.
	"" `match()` returns a 0-based index, so add 1 for the column number.
	"" If no match is found, `match()` returns -1.
	let l:glyph_col=match(getline('.'), '\S') + 1

	" Get the column number of the current cursor position.
	let l:cursor_col=col('.')

	" If visual line is different than actual line, go to start of visual
	" line
	if &wrap && (line('.') != winline())
		let l:movement_string='g^'
	" Handle case where there no glyphs (i.e., the line is empty or
	" entirely whitespace).
	elseif l:glyph_col == 0
		let l:movement_string='0'
	" If the cursor is currently at or before the glyph, go to the beginning
	" of the line.
	elseif l:cursor_col <= l:glyph_col
		let l:movement_string='0'
	endif

	return l:movement_string
endfunction
