source ~/.vim/bundles.vim

" This needs to be after plugins are loaded, otherwise your user stuff may be
" overridden by the defaults from plugins:
filetype plugin indent on

set backspace=indent,eol,start
set expandtab
set noerrorbells
set visualbell t_vb=  " TODO: does t_vb do anything anymore?
if exists('&esckeys')
  set esckeys  " (not in neovim)
endif
set ignorecase
set smartcase
set autowrite
" Ignore compiled files when completing paths:
set wildignore=*.o,*~,*.pyc,*.pyo,*.class,*.hi,*.obj
set hidden
set laststatus=2  " Show the status bar even when there's only one window.
set listchars+=nbsp:␣     " non-breaking spaces (0xA0, U+202F)
set listchars+=extends:›  " at the right edge when text extends offscreen
set listchars+=precedes:‹ " at the left edge when scrolled right
set listchars+=tab:›_
set listchars+=trail:·
set fillchars+=vert:┃
set fillchars+=fold:-
set fillchars+=diff:╳
set showbreak=↳           " at the start of continued wrapped lines
set list                  " Display all the stuff defined in listchars above
set scrolloff=2
" This screws things up when you use >> and <<:
"set shiftround

set backupdir=$HOME/.vim/backup//
set directory=$HOME/.vim/swap//
set undodir=$HOME/.vim/undo//

if has("mouse")
  set mouse=a
endif

set showmatch
set nostartofline
set textwidth=79

set shiftwidth=2  " 2-space indents by default

" Make it so various navigation keys will wrap across line breaks like every
" other editor in the universe:
set whichwrap=b,s,h,l,<,>,[,]

" Don't wrap searches around the end of a file:
"set nowrapscan

" Make a backup before overwriting a file (see :help wb)
"set nowritebackup

" Attempts to make `gf` file jumping work for python imports.
if has("python")
python << EOF
import os
import sys
import vim
for p in sys.path:
  if os.path.isdir(p):
    vim.command(r"set path+=%s" % p.replace(" ", r"\ "))
EOF
endif

" From http://google-styleguide.googlecode.com/svn/trunk/google_python_style.vim
" See also http://google-styleguide.googlecode.com/svn/trunk/pyguide.html

function! GetGooglePythonIndent(lnum)
  " Indent inside parens.
  " Align with the open paren unless it is at the end of the line.
  " E.g.
  "   open_paren_not_at_EOL(100,
  "                         (200,
  "                          300),
  "                         400)
  "   open_paren_at_EOL(
  "       100, 200, 300, 400)
  call cursor(a:lnum, 1)
  let [par_line, par_col] = searchpairpos('(\|{\|\[', '', ')\|}\|\]', 'bW',
        \ "line('.') < " . (a:lnum - s:maxoff) . " ? dummy :"
        \ . " synIDattr(synID(line('.'), col('.'), 1), 'name')"
        \ . " =~ '\\(Comment\\|String\\)$'")
  if par_line > 0
    call cursor(par_line, 1)
    if par_col != col("$") - 1
      return par_col
    endif
  endif

  " Delegate the rest to the original function.
  return GetPythonIndent(a:lnum)
endfunction

augroup myfiletypestuff
  autocmd!
  autocmd FileType mail
        \ setlocal tw=72 nowrap
  autocmd FileType pants
        \ setlocal indentexpr=GetPythonIndent(v:lnum) sw=4
  " Trim trailing whitespace on save: (I can't seem to make this work)
  " autocmd BufWritePre *.py normal m`:%s/\s\+$//e``
  autocmd FileType conf
        \ setlocal foldcolumn=0 modeline modelines=5
  autocmd FileType vim
        \ setlocal shiftwidth=2
  autocmd FileType ruby
        \ setlocal shiftwidth=2
  autocmd FileType gitcommit
        \ setlocal textwidth=71
  autocmd FileType make
        \ setlocal noexpandtab
  autocmd FileType man
        \ setlocal nolist tw=0
  autocmd FileType scheme
        \ setlocal lisp
  " It turns out that this is terrible because it doesn't account for code blocks
  " and intentional linebreaks:
  autocmd FileType markdown
        \ setlocal formatoptions=cqln linebreak wrap tw=0
  autocmd FileType javascript
        \ setlocal shiftwidth=2
  autocmd FileType nix
        \ setlocal smartindent autoindent
  autocmd FileType go
        \ setlocal tabstop=4 shiftwidth=4
  autocmd FileType python
        \ setlocal tags+=$HOME/.vim/tags/python27.tags
        \     indentexpr=GetGooglePythonIndent(v:lnum)
        \     shiftwidth=4
        \     formatprg=autopep8\ -
        \     omnifunc=pythoncomplete#Complete
  autocmd FileType haskell
        \ setlocal shiftwidth=2 tw=0
  autocmd FileType bzl
        \ setlocal shiftwidth=2
augroup end

let s:maxoff = 50 " maximum number of lines to look backwards.

augroup colorstuff
  autocmd!
  autocmd ColorScheme * highlight OverLength ctermfg=red guifg=red

  " http://vim.wikia.com/wiki/Highlight_unwanted_spaces
  autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
  autocmd ColorScheme * match ExtraWhitespace /\s\+$/
  autocmd InsertEnter * match ExtraWhitespace /\s\+%#\@<!$/
  autocmd InsertLeave * match ExtraWhitespace /\s\+$/
augroup end

" ConqueTerm is more than willing to use vim syntax highlighting, but it makes
" shells really quite slow.
let g:ConqueTerm_Color=0

" Bundle 'Tagbar'
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

let g:haskell_conceal = 0
let g:haskell_conceal_wide = 0

" Default shell syntax variant when the shebang line isn't explicit:
let g:is_bash = 1

" syntax on

" This works with Konsole, not sure about other terminals:
set termguicolors
colorscheme base16-materia

"colorscheme zenburn
" Looks nice with zenburn:
"highlight ColorColumn ctermbg=238 guibg=#484848

let g:syntastic_check_on_open = 1
"let g:syntastic_error_symbol = '✗'
"let g:syntastic_warning_symbol = '⚠'
let g:syntastic_ruby_checkers = ['rubocop', 'mri']
let g:syntastic_aggregate_errors = 1
let g:syntastic_auto_loc_list = 2
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_jsonnet_checkers = ['jsonnet']

let g:virtualenv_stl_format='[%n]'  " TODO: Does this do anything with airline installed?
"set statusline=%<%f\ %h%m%r%{VirtualEnvStatusline()}%=%-14.(%l,%c%V%)\ %P

let g:NERDTreeIgnore=['\~$', '\.pyc$', 'bazel-\w\+$[[dir]]']
let g:NERDTreeHijackNetrw=1  " TODO: what does this actually accomplish?
let g:NERDTreeQuitOnOpen=1

" Please don't resize my windows and move everything around stupidly
let g:buffergator_autoexpand_on_split = 0

" Horizontal split, buffer list on top 10 lines
let g:buffergator_viewport_split_policy = "T"
let g:buffergator_hsplit_size = 10

" Show relative paths in the second column
let g:buffergator_show_full_directory_path = 0

" Show line numbers relative to cursor position
set relativenumber
augroup relnum703
  autocmd! BufReadPost * set relativenumber
augroup end

" Color current line number in obnoxious magenta
"highlight CursorLineNr ctermfg=130 guifg=#ff5fd7

" Subtly show column 80
set colorcolumn=+1
"highlight ColorColumn ctermbg=238 guibg=#121212

if has("mac")
  let g:ycm_path_to_python_interpreter = '/usr/bin/python'
endif

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

augroup mytemplatestuff
  " add templates in templates/ using filetype as file name
  autocmd! BufNewFile * :silent! exec ":0r ".$VIMHOME."templates/".&ft
augroup end

nnoremap <silent> <Leader>e :Errors<CR>
nnoremap <Leader>s :SyntasticToggleMode<CR>
nnoremap <silent> <Leader>r :TagbarToggle<CR>
nnoremap <silent> <Leader>n :NERDTreeToggle<CR>
nnoremap <silent> <Leader>b :BuffergatorToggle<CR>

let pyindent_nested_paren = 4
let pyindent_open_paren = 4

""" Stuff I was using during mystery hunt:
" noremap  <buffer> <silent> k gk
" noremap  <buffer> <silent> j gj
" noremap  <buffer> <silent> ^ g^
" noremap  <buffer> <silent> $ g$
"
" noremap  <buffer> <silent> <Up>   gk
" noremap  <buffer> <silent> <Down> gj
" noremap  <buffer> <silent> <Home> g<Home>
" noremap  <buffer> <silent> <End>  g<End>
"
" inoremap <buffer> <silent> <Up>   <C-o>gk
" inoremap <buffer> <silent> <Down> <C-o>gj
" inoremap <buffer> <silent> <Home> <C-o>g<Home>
" inoremap <buffer> <silent> <End>  <C-o>g<End>
"
" noremap <buffer> <silent> , :s<cr>

let g:jsonnet_fmt_on_save = 0

" allows cursor change in tmux mode, but wreaks havoc with konsole fonts
" if exists('$TMUX')
"     let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
"     let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
" else
"     let &t_SI = "\<Esc>]50;CursorShape=1\x7"
"     let &t_EI = "\<Esc>]50;CursorShape=0\x7"
" endif

set secure
