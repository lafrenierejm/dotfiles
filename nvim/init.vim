" Disable modeline for better security
set nomodeline

" Per-OS changes
if has('win32')
	" Default location of config
	let g:NVIM_CONFIG_HOME='~/vimfiles'
	" Use system clipboard
	set clipboard+=unnamedplus
else
	let g:NVIM_CONFIG_HOME=$XDG_CONFIG_HOME.'/nvim'
	" Use system clipboard
	if has('clipboard')
		set clipboard+=unnamedplus
	endif
endif

" Look for a readable instance of Vim-Plug
if filereadable(expand(g:NVIM_CONFIG_HOME.'/autoload/plug.vim'))
	call plug#begin(g:NVIM_CONFIG_HOME . '/plugged')
		Plug 'AndrewRadev/splitjoin.vim'                  " Switch between multi- and single-line statements
		Plug 'PProvost/vim-ps1'                           " Syntax for powershell scripts ( . ps1)
		Plug 'amirh/HTML-AutoCloseTag', { 'for': 'html' } " Auto close html tags
		Plug 'cespare/vim-toml'                           " Syntax for TOML
		Plug 'junegunn/vim-easy-align'                    " Easy column-based alignment
		Plug 'neomake/neomake'                            " Async :make and linting framework
		Plug 'rust-lang/rust.vim'                         " Configuration for Rust
		Plug 'tpope/vim-commentary'                       " Comment and uncomment easily
		Plug 'tpope/vim-repeat'                           " `.` support for plugins
		Plug 'tpope/vim-surround'                         " Quoting and parenthesizing
	call plug#end()
endif

" Temporary files
"" Backups
execute 'set backupdir='.$XDG_DATA_HOME.'/nvim/backup//'
"" Swap files
execute 'set directory='.$XDG_DATA_HOME.'/nvim/swap//'
"" Undo
execute 'set undodir='.$XDG_DATA_HOME.'/nvim/undo//'
set undofile

" Character encoding
set encoding=utf-8

" Disable mouse mode
if has('mouse')
	set mouse=""
endif

" Allow backspacing over autoindent, line breaks, start of insert action
set backspace=indent,eol,start

" Always show the ruler
set ruler

" Show quickfix window
au QuickFixCmdPost [^l]* nested cwindow
au QuickFixCmdPost    l* nested lwindow

" No bells
set belloff=all

" Get out of insert mode faster
set notimeout
set ttimeout
set ttimeoutlen=50 " Key code delay

" Indentation settings
set tabstop=4             " Tabs stop at multiples of this column number
set shiftwidth=4          " Number of spaces to use for each step of indentation
set softtabstop=4         " Number of spaces a tab counts for when editing
filetype indent plugin on " Attempt to indent by filetype
set autoindent            " Default to same indent as current line

" Highlight matching {[(
set showmatch

" Splitting
set splitright " Horizontal split will go right
set splitbelow " Vertical split will go below

" Folding
if has('folding')
	set foldenable        " Enable folding
	set foldnestmax=10    " Limit maximum embedded folds to 10
	set foldmethod=syntax " Fold based on syntax
endif

" Switch buffers without requiring that buffers be saved
set hidden
