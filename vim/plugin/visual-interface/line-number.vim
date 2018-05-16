" Hybrid line numbers upon buffer onpen
set relativenumber

" Disable hybrid numbering in insert mode
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" Always have static numbering
set number
