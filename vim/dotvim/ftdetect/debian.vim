augroup filetypedetect
  au BufNewFile,BufRead * if expand("%:p") =~ 'debian/patches' | set filetype=diff | endif
  au BufNewFile,BufRead series if expand("%:p") =~ 'debian/patches' | set filetype=conf | endif
augroup END
