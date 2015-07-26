" $Id: //depot/ops/monitoring/mongoogle/contrib/vim/filetype.vim#1 $
" This tries to identify mongoogle files by their pathname
" Put this in your ~/.vim directory
augroup filetypedetect
   au BufNewFile,BufRead *.conf if expand("%:p") =~ 'monitoring/mongoogle/conf' | set filetype=mongoogle | endif
augroup END
