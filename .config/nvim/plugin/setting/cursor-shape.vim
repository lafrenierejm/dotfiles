" Change the cursor shape per mode

" Use Neovim's built-in cursor change
if has('nvim')
	" https://github.com/neovim/neovim/wiki/Following-HEAD#20170402
	set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
" Change the cursor shape for VTE-compatible terminals
"" Allow for the running of TMUX by surrounding with DCS sequence
"" https://gist.github.com/andyfowler/1195581
elseif exists('$TMUX') && ($TERMINAL =~# 'rxvt') " Allow for TMUX
	let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>[6 q\<Esc>\\" " INSERT - solid vertical bar
	let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>[4 q\<Esc>\\" " REPLACE - solid underbar
	let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>[2 q\<Esc>\\" " All others - solid block
elseif (&term =~# 'rxvt')
	let &t_SI = "\<Esc>[6 q" " INSERT - solid vertical bar
	let &t_SR = "\<Esc>[4 q" " REPLACE - solid underbar
	let &t_EI = "\<Esc>[2 q" " All others - solid block
endif
