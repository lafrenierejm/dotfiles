" Disable modeline for better security
set nomodeline

" Per-OS changes
if has('win32')
	" set the configuration directory
	let g:VIM_CONFIG_HOME='~/vimfiles'
	" use system clipboard
	set clipboard+=unnamedplus
else
	" set the configuration directory
	if has('nvim')
		let g:VIM_CONFIG_HOME=$HOME.'/.config/nvim'
	else
		let g:VIM_CONFIG_HOME=$HOME.'/.vim'
	endif
	" use system clipboard if running in X
	if has('clipboard') && $TERM !~# 'linux'
		set clipboard+=unnamedplus
	endif
endif

" Look for a readable instance of Vim-Plug
if filereadable(expand(g:VIM_CONFIG_HOME.'/autoload/plug.vim'))
	call plug#begin(g:VIM_CONFIG_HOME.'/plugged')
		Plug 'https://github.com/AndrewRadev/splitjoin.vim.git' " Switch between multi- and single-line statements
		Plug 'https://github.com/PProvost/vim-ps1.git' " Syntax for powershell scripts ( . ps1)
		Plug 'https://github.com/airblade/vim-gitgutter.git' " show git diff and stage/unstage hunks
		Plug 'https://github.com/amirh/HTML-AutoCloseTag', { 'for': 'html.git' } " Auto close html tags
		Plug 'https://github.com/ap/vim-css-color.git' " show corresponding colors in source
		Plug 'https://github.com/cespare/vim-toml.git' " Syntax for TOML
		Plug 'https://github.com/junegunn/vim-easy-align.git' " Easy column-based alignment
		Plug 'https://github.com/neomake/neomake.git' " Async :make and linting framework
		Plug 'https://github.com/rust-lang/rust.vim.git' " Configuration for Rust
		Plug 'https://github.com/tpope/vim-commentary.git' " Comment and uncomment easily
		Plug 'https://github.com/tpope/vim-fugitive.git' " a git porcelain
		Plug 'https://github.com/tpope/vim-repeat.git' " `.` support for plugins
		Plug 'https://github.com/tpope/vim-surround.git' " Quoting and parenthesizing
		Plug 'https://github.com/wogong/msmtp.vim.git' " msmtp syntax highlighting
		Plug 'https://gitlab.com/lafrenierejm/vim-format-flowed.git' " dynamically set mail formatoptions
		Plug 'https://gitlab.com/lafrenierejm/vim-monokai.git' " monokai color scheme
	call plug#end()
endif

" Temporary files
"" Backups
execute 'set backupdir='.$HOME.'/.local/share/nvim/backup//'
"" Swap files
execute 'set directory='.$HOME.'/.local/share/nvim/swap//'
"" Undo
execute 'set undodir='.$HOME.'/.local/share/nvim/undo//'
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
