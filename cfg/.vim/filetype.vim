if exists("did_load_filetypes")
  finish
endif

""" Begin paste copied from /usr/share/vim/vim73/filetype.vim:
" Pattern used to match file names which should not be inspected.
" Currently finds compressed files.
if !exists("g:ft_ignore_pat")
  let g:ft_ignore_pat = '\.\(Z\|gz\|bz2\|zip\|tgz\)$'
endif

" Function used for patterns that end in a star: don't set the filetype if the
" file name matches ft_ignore_pat.
fun! s:StarSetf(ft)
  if expand("<amatch>") !~ g:ft_ignore_pat
    exe 'setf ' . a:ft
  endif
endfunc
""" End paste

augroup filetypedetect
  " Handle more filenames for bind configs and zone files
  au! BufNewFile,BufRead named.conf.*,zones.*
        \ call s:StarSetf('named')
  au! BufNewFile,BufRead */zones/db.*
        \ call s:StarSetf('bindzone')

  au! BufNewFile,BufRead /etc/nginx/*,/usr/local/nginx/conf/*
        \ if &ft=='' | setfiletype nginx | endif

  au! BufNewFile,BufRead /etc/haproxy/haproxy*
        \ setfiletype haproxy

  au! BufRead,BufNewFile *.pp
        \ setfiletype puppet

  au! BufRead,BufNewFile Puppetfile
        \ setfiletype ruby

  au! BufRead,BufNewfile */debian/upstart,*/debian/*.upstart
        \ setfiletype upstart

  au! BufRead,BufNewfile *.jsonnet
        \ set filetype=jsonnet sw=2 ai si
augroup end
