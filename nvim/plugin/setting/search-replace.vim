set gdefault   " use [g] s_flag to replace all occurrences in line by default
set ignorecase " No case sensitivity when searching
set incsearch  " Incrementally search while typing
set smartcase  " Case sensitive only when there is a captial letter in search
set wrapscan   " Wrap to beginning of file if end is reached while searching
if exists('&inccommand')
	set inccommand=nosplit " show effect of commands as typed
endif
