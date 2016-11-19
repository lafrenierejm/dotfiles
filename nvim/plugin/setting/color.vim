" Use 256 color mode
set t_Co=256

" Disable background color erase
set t_ut=

" Set the colorscheme
let g:base16colorspace=256      " Access colors present in 256 colorspace
colorscheme base16-default-dark " chriskempson's Base16 Default Dark

" Enable syntax highlighting
syntax enable

" Make line numbers grey
highlight LineNr ctermfg=grey
highlight CursorLineNr ctermfg=grey
