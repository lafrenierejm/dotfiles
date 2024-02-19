function! ShowFileFormatFlag()
	if(&fileformat ==# 'dos')
		return '[dos]'
	elseif(&fileformat ==# 'mac')
		return '[mac]'
	else
		return '[unix]'
	endif
endfunction

set statusline=%< " truncate line here if too long
set statusline+=%f " path of file in the buffer
set statusline+=%m " modified flag
set statusline+=%r " readonly flag
set statusline+=%= " break alignment of statusline

set statusline+=%{ShowFileFormatFlag()}
set statusline+=%y " filetype
set statusline+=[ " literal left bracket
set statusline+=%l " line number
set statusline+=/ " literal forward slash
set statusline+=%L " number of lines in buffer
set statusline+=, " literal comma
set statusline+=%c " column number
set statusline+=%V " virtual column number
set statusline+=] " literal right bracket
