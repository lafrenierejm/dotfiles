" Disable modeline for better security
set nomodeline

" operating system-specific changes
if has('win32')
	" configuration file location
	let g:CONFIG_HOME='~/vimfiles'
	" use system clipboard
	set clipboard+=unnamedplus
else
	if has('nvim')
		" configuration file location
		let g:CONFIG_HOME=$HOME.'/.config/nvim'
		let g:DATA_HOME=$HOME.'/.local/share/nvim'
		" backup file location
		execute 'set backupdir='.g:DATA_HOME.'/backup//'
		call mkdir(&backupdir, 'p', 0700)
		" swap file location
		execute 'set directory='.g:DATA_HOME.'/swap//'
		call mkdir(&directory, 'p', 0700)
		" undo file location
		if has('persistent_undo')
			execute 'set undodir='.g:DATA_HOME.'/undo//'
			call mkdir(&undodir, 'p', 0700)
			set undofile
		endif
	else
		" configuration file location
		let g:CONFIG_HOME=$HOME.'/.vim'
		let g:DATA_HOME=$HOME.'/.local/share/vim'
		" backup file location
		execute 'set backupdir='.g:DATA_HOME.'/backup//'
		call mkdir(&backupdir, 'p', 0700)
		" swap file location
		execute 'set directory='.g:DATA_HOME.'/swap//'
		call mkdir(&directory, 'p', 0700)
		" undo file location
		if has('persistent_undo')
			execute 'set undodir='.g:DATA_HOME.'/undo//'
			call mkdir(&undodir, 'p', 0700)
			set undofile
		endif
	endif
	" use system clipboard if running in X
	if has('clipboard') && ($DISPLAY != '')
		set clipboard+=unnamedplus
	endif
endif

" Look for a readable instance of Vim-Plug
if filereadable(expand(g:CONFIG_HOME.'/autoload/plug.vim'))
	call plug#begin(g:CONFIG_HOME.'/plugged')
		Plug 'https://github.com/AndrewRadev/splitjoin.vim.git' " Switch between multi- and single-line statements
		Plug 'https://github.com/PProvost/vim-ps1.git' " Syntax for powershell scripts ( . ps1)
		Plug 'https://github.com/airblade/vim-gitgutter.git' " show git diff and stage/unstage hunks
		Plug 'https://github.com/ap/vim-css-color.git' " show corresponding colors in source
		Plug 'https://github.com/cespare/vim-toml.git' " Syntax for TOML
		Plug 'https://github.com/dkarter/bullets.vim' " Automatic bulleted lists
		Plug 'https://github.com/junegunn/vim-easy-align.git' " Easy column-based alignment
		Plug 'https://github.com/kana/vim-textobj-indent.git' " indentation text objects (i)
		Plug 'https://github.com/kana/vim-textobj-user.git' " dependency for custom text objects
		Plug 'https://github.com/lafrenierejm/wstrip.vim.git', " strip trailing whitespace on changed lines
		Plug 'https://github.com/neomake/neomake.git' " Async :make and linting framework
		Plug 'https://github.com/rust-lang/rust.vim.git' " Configuration for Rust
		Plug 'https://github.com/tpope/vim-commentary.git' " Comment and uncomment easily
		Plug 'https://github.com/tpope/vim-repeat.git' " `.` support for plugins
		Plug 'https://github.com/tpope/vim-surround.git' " Quoting and parenthesizing
		Plug 'https://github.com/wogong/msmtp.vim.git' " msmtp syntax highlighting
		Plug 'https://gitlab.com/lafrenierejm/vim-equivalence.git' " search for character equivalencies
		Plug 'https://gitlab.com/lafrenierejm/vim-format-flowed.git' " dynamically set mail formatoptions
		Plug 'https://gitlab.com/lafrenierejm/vim-monokai.git' " monokai color scheme
	call plug#end()
endif

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

" Switch buffers without requiring that buffers be saved
set hidden
