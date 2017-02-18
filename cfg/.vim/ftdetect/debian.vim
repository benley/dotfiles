au BufNewFile,BufRead series if expand("%:p") =~ 'debian/patches' | setfiletype conf | endif
au BufNewFile,BufRead * if expand("%:p") =~ 'debian/patches' | setfiletype diff | endif
