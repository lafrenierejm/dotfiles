" Bind 'ga' to EasyAlign
function! BindEasyAlign()
	" Test for the EasyAlign command
	if exists(':EasyAlign')
		" Start interactive EasyAlign in visual mode (e.g. 'vipga')
		xmap ga <Plug>(EasyAlign)
		" Start interactive EasyAlign for a motion/text object (e.g. 'gaip')
		nmap ga <Plug>(EasyAlign)
	endif
endfunction
autocmd VimEnter * call BindEasyAlign()
