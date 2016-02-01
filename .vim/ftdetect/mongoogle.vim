" This is long dead but kinda nostalgic
"
au BufNewFile,BufRead *.conf
   \ if expand("%:p") =~ 'monitoring/mongoogle/conf'
   \ | setfiletype mongoogle
   \ | endif
