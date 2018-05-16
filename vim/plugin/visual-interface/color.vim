" Use 256 color mode
set t_Co=256

" Disable background color erase
set t_ut=

" Set the color theme to be used
colors default

" Enable syntax highlighting
if has('syntax') && !exists('g:syntax_on')
	syntax enable
endif

" Make line numbers grey
highlight LineNr ctermfg=grey
highlight CursorLineNr ctermfg=grey
