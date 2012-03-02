set nocompatible
set backspace=indent,eol,start
set expandtab
set noerrorbells
set esckeys
set incsearch
" This will highlight all matches from the previous search.
" set hlsearch
" set ignorecase
set smartcase
set joinspaces

call pathogen#infect()

if has("mouse")
  set mouse=a
endif

"  set   autowrite
"  set  nobackup
"  set   hidden
"  set   highlight=8r,db,es,hs,mb,Mr,nu,rs,sr,tb,vr,ws
  set   magic
"  set   modelines=1    default is 5
  set   ruler
"set   shiftwidth=4
  set   showcmd
  set   showmatch
  set   showmode
  set   nostartofline
  set   tabstop=8
  set   textwidth=78
  set   visualbell t_vb=
"       much more precise without these.  only practical in a few situations, so
"       leave it here for easy editing
set   whichwrap=b,s,h,l,<,>,[,]
  "set  nowrapscan
  set   nowritebackup

" autocmd!

"map   K  gq

autocmd FileType c,cpp set ts=4 formatoptions=cro cindent smarttab tw=0 nowrap sidescroll=20 listchars=extends:$
autocmd FileType php3,php4 set ts=4 formatoptions=cro smartindent smarttab tw=0 nowrap sidescroll=20 listchars=extends:$
autocmd FileType c,cpp nmap <tab> =0<CR>
autocmd FileType mail set tw=72 nowrap
autocmd FileType py set autoindent

"if &term=="rxvt"
"        set t_Co=8
"        set t_Sf=^[[3%dm
"        set t_Sb=^[[4%dm
"        set t_AB=^[[%?%p1%{8}%<%t%p1%{40}%+%e%p1%{92}%+%;%dm
"        set t_AF=^[[%?%p1%{8}%<%t%p1%{30}%+%e%p1%{82}%+%;%dm
"endif

"highlight RedundantSpaces ctermbg=red guibg=red
"match RedundantSpaces /\s\+$\| \+\ze\t/

"source /home/build/public/eng/vim/google.vim
"source /google/src/head/depot/eng/vim/google.vim
"source /usr/share/vim/google/google.vim

"runtime perforce/perforcemenu.vim

"source /home/build/nonconf/google3/tools/tags/gtags.vim

if &diff
  set lines=50
  set columns=180
endif

fun! FixDescription()
  if search("<enter description here>") > 0
    normal C
    startins!
  elseif search("^Description:")
    normal 2w
  endif
endfun

augroup filetypedetect
  au BufNewFile,BufRead /tmp/*[pg]4[_-]* setl wrap noet tw=65 | call FixDescription()
augroup end

" obnoxious crosshairs thing
"if version >= 700
"  hi CursorColumn term=reverse ctermbg=grey
"  hi clear CursorLine
"  hi CursorLine term=reverse ctermbg=grey
"  hi LineNr ctermfg=red ctermbg=black
"  se cul cuc
"endif

"source /google/src/head/depot/google3/util/shell/gbash/tools/vim/autogen.vim

"source /google/src/head/depot/google3/tools/gsearch/contrib/csearch.vim

autocmd FileType python set omnifunc=pythoncomplete#Complete

if has("syntax")
"        so $VIM/vim58/syntax/syntax.vim
  set background=dark
  syntax on
"       hi! Comment  term=bold  ctermfg=cyan  guifg=Blue
  let g:solarized_termtrans=1
  colorscheme solarized
endif
