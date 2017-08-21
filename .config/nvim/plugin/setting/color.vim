" Use true color if available
if has('termguicolors') && ($DISPLAY != '')
	set termguicolors
	colorscheme monokai
else
" Otherwise use 256 color mode
	set t_Co=256
endif

" Disable background color erase
set t_ut=

" Enable syntax highlighting
syntax enable
