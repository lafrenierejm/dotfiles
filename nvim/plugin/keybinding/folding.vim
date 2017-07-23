" http://vim.wikia.com/wiki/Folding
if has('folding')
	nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
	vnoremap <Space> zf
endif
