source ~/.vim/bundles.vim

set backspace=indent,eol,start
set expandtab
set noerrorbells
set esckeys
set incsearch
" This will highlight all matches from the previous search.
" set hlsearch
set ignorecase
set smartcase
set joinspaces
set autowrite
" Ignore compiled files when completing paths:
set wildignore=*.o,*~,*.pyc,*.pyo,*.class,*.hi,*.obj
set wildmenu
"set nobackup
set hidden
set magic
set laststatus=2 " Keep the status bar even when there's only one window.
set listchars=tab:⇥\   " Loudly highlight the damn tabs
set fillchars=vert:┃,fold:-,diff:╳
" Show invisible characters (e.g. tabs)
set list
set scrolloff=2
" This screws things up when you use >> and <<:
"set shiftround

set backupdir=$HOME/.vim/backup//
set directory=$HOME/.vim/swap//
set undodir=$HOME/.vim/undo//

if has("mouse")
  set mouse=a
endif

"  set   autowrite
"  set   nobackup
"  set   hidden
"  set   highlight=8r,db,es,hs,mb,Mr,nu,rs,sr,tb,vr,ws
"  set   modelines=1    default is 5
set   ruler
"set   shiftwidth=4
set   showcmd
set   showmatch
set   showmode
set   nostartofline
"set   tabstop=8
set   textwidth=79
set   visualbell t_vb=
"       much more precise without these.  only practical in a few situations, so
"       leave it here for easy editing
set   whichwrap=b,s,h,l,<,>,[,]
"set  nowrapscan
set   nowritebackup

" autocmd!

"map   K  gq

set nonumber

"autocmd FileType c,cpp set ts=4 formatoptions=cro cindent smarttab tw=0 nowrap sidescroll=20 listchars=extends:$
"autocmd FileType php3,php4 set ts=4 formatoptions=cro smartindent smarttab tw=0 nowrap sidescroll=20 listchars=extends:$
"autocmd FileType c,cpp nmap <tab> =0<CR>
autocmd FileType mail
      \ setlocal tw=72 nowrap
autocmd FileType pants setl indentexpr=GetPythonIndent(v:lnum) sw=4
" Trim trailing whitespace on save: (I can't seem to make this work)
" autocmd BufWritePre *.py normal m`:%s/\s\+$//e``
autocmd FileType conf
      \ setlocal foldcolumn=0 modeline modelines=5
autocmd FileType sh
      \ call BenCode()
      \ | setlocal shiftwidth=2
      \ | setlocal formatprg=
autocmd FileType vim
      \ call BenCode()
      \ | setlocal shiftwidth=2
autocmd FileType ruby
      \ call BenCode()
      \ | setlocal shiftwidth=2
autocmd FileType gitcommit
      \ setlocal textwidth=71
autocmd FileType make
      \ setlocal noexpandtab
autocmd FileType yaml
      \ setlocal shiftwidth=2 tabstop=2 modeline modelines=5 autoindent
autocmd FileType man
      \ setlocal nolist tw=0
autocmd FileType scheme
      \ setlocal lisp
" It turns out that this is terrible because it doesn't account for code blocks
" and intentional linebreaks:
"autocmd FileType markdown
"      \ setlocal formatoptions+=a
autocmd FileType javascript
      \ setlocal shiftwidth=2
autocmd FileType nix
      \ setlocal smartindent autoindent
autocmd FileType go
      \ setlocal listchars=tab:\ \  tabstop=4 shiftwidth=4

fun! BenCode()
  "I hate this right now for some reason
  "setlocal colorcolumn=+1,+2 foldcolumn=5 foldmethod=indent number foldnestmax=4
  match OverLength /\%>80v.\+/
  "setlocal shiftwidth=2 smartindent autoindent expandtab
  setlocal shiftwidth=4 expandtab
endfun

"highlight RedundantSpaces ctermbg=red guibg=red
"match RedundantSpaces /\s\+$\| \+\ze\t/

" Oh man this is annoying. Resizes your window. Maybe okay in gui mode?
"if &diff
"  set lines=50
"  set columns=180
"endif

" This was for perforce or something.
fun! FixDescription()
  if search("<enter description here>") > 0
    normal C
    startins!
  elseif search("^Description:")
    normal 2w
  endif
endfun

""" Begin paste copied from /usr/share/vim/vim73/filetype.vim:
" Pattern used to match file names which should not be inspected.
" Currently finds compressed files.
if !exists("g:ft_ignore_pat")
  let g:ft_ignore_pat = '\.\(Z\|gz\|bz2\|zip\|tgz\)$'
endif

" Function used for patterns that end in a star: don't set the filetype if the
" file name matches ft_ignore_pat.
func! s:StarSetf(ft)
  if expand("<amatch>") !~ g:ft_ignore_pat
    exe 'setf ' . a:ft
  endif
endfunc
""" End paste

augroup filetypedetect
  " auto handling for perforce commit messages:
  au BufNewFile,BufRead /tmp/*[pg]4[_-]*
        \ setlocal wrap noexpandtabs tw=65 | call FixDescription()
  " Handle more filenames for bind configs and zone files
  au BufNewFile,BufRead named.conf.*,zones.*
        \ call s:StarSetf('named')
  au BufNewFile,BufRead */zones/db.*
        \ call s:StarSetf('bindzone')
  " Match nginx configs
  au BufNewFile,BufRead /etc/nginx/*,/usr/local/nginx/conf/*
        \ if &ft=='' | setfiletype nginx | endif
  au BufNewFile,BufRead /etc/haproxy/haproxy*
        \ setlocal filetype=haproxy
augroup end

" obnoxious crosshairs thing
"if version >= 700
"  hi CursorColumn term=reverse ctermbg=grey
"  hi clear CursorLine
"  hi CursorLine term=reverse ctermbg=grey
"  hi LineNr ctermfg=red ctermbg=black
"  set cursorline cursorcolumn
"endif

autocmd ColorScheme * highlight OverLength ctermfg=red guifg=red

" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd ColorScheme * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

" ConqueTerm is more than willing to use vim syntax highlighting, but it makes
" shells really quite slow.
let g:ConqueTerm_Color=0

" Bundle 'Tagbar'
nnoremap <silent> <F9> :TagbarToggle<CR>
set title titlestring=%<%f\ %(%{tagbar#currenttag('[%s]','','s')}%)
if has("unix")
  let s:uname = system("uname")
  if s:uname == "Darwin\n"
    if filereadable("/usr/local/bin/ctags")
      let g:tagbar_ctags_bin = "/usr/local/bin/ctags"
    elseif filereadable("/opt/local/bin/ctags")
      let g:tagbar_ctags_bin = "/opt/local/bin/ctags"
    endif
  endif
endif

" https://github.com/majutsushi/tagbar/wiki
" (requires custom stuff in ~/.ctags as well)
let g:tagbar_type_markdown = {
      \ 'ctagstype': 'markdown',
      \ 'kinds': ['h:Heading_L1',
      \           'i:Heading_L2',
      \           'k:Heading_L3'
      \          ]}


"Bundle "taglist.vim"
"if has("unix")
"  let s:uname = system("uname")
"  if s:uname == "Darwin\n"
"    if filereadable("/opt/local/bin/ctags")
"      let Tlist_Ctags_Cmd = "/opt/local/bin/ctags"
"    elseif filereadable("/usr/local/bin/ctags")
"      let Tlist_Ctags_Cmd = "/usr/local/bin/ctags"
"    endif
"  endif
"endif

" Show the tags menu in gvim.
"let Tlist_Show_Menu = 1

" Make TagList process files even if the taglist window isn't open.
"let Tlist_Process_File_Always = 1

" Set the terminal window title bar to be the current tag. Awesome.
" (this benefits from Tlist_Process_File_Always=1)
"set title titlestring=%<%f\ %([%{Tlist_Get_Tagname_By_Line()}]%)

" WinManager blahblah
let g:winManagerWindowLayout = "BufExplorer,FileExplorer|TagList"

if has("gui")
  " I don't know why this doesn't work when quoted.
  "set guifont=Inconsolata:h16
  "set guifont=Monofur\ 13
  if has("macunix")
    set guifont=Meslo\ LG\ S\ for\ Powerline:h12,Menlo\ Regular\:h12
  else
    set guifont=Ubuntu\ Mono\ derivative\ Powerline\ 11,Ubuntu\ Mono\ 11
  endif
  "set bg=dark

  " mac and unix defaults:
  "set guioptions=aegimrLtT
  " e : gui tabs
  " T : toolbar
  " m : menu
  set guioptions=agirLt

  let g:haskell_conceal_wide = 1
endif

let g:syntastic_check_on_open = 1
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_ruby_checkers = ['rubocop', 'mri']
let g:syntastic_aggregate_errors = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_always_populate_loc_list = 1

syntax on
set bg=dark
colorscheme zenburn
" Looks nice with zenburn:
highlight ColorColumn ctermbg=238 guibg=#484848

let g:virtualenv_stl_format='[%n]'
"set statusline=%<%f\ %h%m%r%{VirtualEnvStatusline()}%=%-14.(%l,%c%V%)\ %P

let g:NERDTreeIgnore=['\~$', '\.pyc$']

if version >= 703
  " Show line numbers relative to cursor position
  set relativenumber
  au BufReadPost * set relativenumber

  " Color current line number in obnoxious magenta
  "highlight CursorLineNr ctermfg=130 guifg=#ff5fd7

  " Subtly show column 80
  set colorcolumn=+1
  "highlight ColorColumn ctermbg=238 guibg=#121212
endif

" http://vim.wikia.com/wiki/View_man_pages_in_Vim
runtime ftplugin/man.vim

let g:ycm_filetype_blacklist = {
      \ 'notes': 1,
      \ 'markdown': 1,
      \ 'unite': 1,
      \ 'tagbar': 1,
      \ 'pandoc': 1,
      \ 'qf': 1,
      \ 'vimwiki': 1,
      \ 'text': 1,
      \ 'infolog': 1,
      \ 'mail': 1,
      \ 'yaml': 1,
      \ 'clojure': 1
      \ }

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#left_sep = ' '
" let g:airline#extensions#tabline#left_alt_sep = '┃'

if has("win32") || has ('win64')
    let $VIMHOME = $HOME."/vimfiles/"
else
    let $VIMHOME = $HOME."/.vim/"
endif

" add templates in templates/ using filetype as file name
au BufNewFile * :silent! exec ":0r ".$VIMHOME."templates/".&ft

map <silent> <Leader>e :Errors<CR>
map <Leader>s :SyntasticToggleMode<CR>

set secure
