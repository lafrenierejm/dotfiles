" Change the cursor shape for VTE-compatible terminals
"" Allow for the running of TMUX by surrounding with DCS sequence
"" https://gist.github.com/andyfowler/1195581
if exists('$TMUX') && ($TERMINAL =~ 'rxvt') " Allow for TMUX
	let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>[6 q\<Esc>\\" " INSERT - solid vertical bar
	let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>[4 q\<Esc>\\" " REPLACE - solid underbar
	let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>[2 q\<Esc>\\" " All others - solid block
elseif (&term =~ 'rxvt')
	let &t_SI = "\<Esc>[6 q" " INSERT - solid vertical bar
	let &t_SR = "\<Esc>[4 q" " REPLACE - solid underbar
	let &t_EI = "\<Esc>[2 q" " All others - solid block
endif
