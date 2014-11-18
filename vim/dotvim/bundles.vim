set nocompatible | filetype indent plugin on | syn on

fun! SetupVAM()
  let c = get(g:, 'vim_addon_manager', {})
  let g:vim_addon_manager = c
  let c.plugin_root_dir = expand('$HOME', 1) . '/.vim/vim-addons'
  " most used options you may want to use:
  " let c.log_to_buf = 1
  " let c.auto_install = 0
  let &rtp.=(empty(&rtp)?'':',').c.plugin_root_dir.'/vim-addon-manager'
  if !isdirectory(c.plugin_root_dir.'/vim-addon-manager/autoload')
    execute '!git clone --depth=1 git://github.com/MarcWeber/vim-addon-manager '
        \       shellescape(c.plugin_root_dir.'/vim-addon-manager', 1)
  endif

  " This provides the VAMActivate command. You could be passing plugin names, too
  call vam#ActivateAddons([], {})
endfun
call SetupVAM()

call vundle#rc()

VAMActivate vim-fugitive
VAMActivate vim-eunuch
VAMActivate vim-jdaddy
VAMActivate vimux
"Bundle 'fholgado/minibufexpl.vim'
VAMActivate genutils
"Bundle 'instant-markdown.vim'

" Color schemes:
"Bundle 'Ambient-Color-Scheme'
"Bundle 'clarity.vim'
"Bundle 'oceandeep'
Bundle 'oceanlight'
"Bundle 'Cthulhian'
VAMActivate Zenburn
VAMActivate Solarized
VAMActivate darktango
Bundle 'morhetz/gruvbox'
"Bundle 'chriskempson/base16-vim'
Bundle 'jonathanfilip/vim-lucius'
Bundle 'tomasr/molokai'
Bundle 'sickill/vim-monokai'

"Bundle 'Conque-Shell'
Bundle 'rosenfeld/conque-term'
VAMActivate pythoncomplete
Bundle 'rogerz/vim-json'
VAMActivate github:scrooloose/nerdtree
VAMActivate Tagbar

VAMActivate taglist
VAMActivate Syntastic

" I think this does some messy stuff that interferes with things other than
" Python. Grr.
"Bundle 'python_ifold'

VAMActivate Supertab
Bundle 'depuracao/vim-rdoc'

"Bundle 'RDoc'
"Bundle 'confluencewiki.vim'
VAMActivate gitv
"Bundle 'pydoc.vim'
"VAMActivate pydoc

" Syntax highlighting:
Bundle 'pantsbuild/vim-pants'
Bundle 'ekalinin/Dockerfile.vim'
VAMActivate nginx
"Bundle 'fish.vim'
VAMActivate vim-coffee-script
" http://www.vim.org/scripts/script.php?script_id=790 :
Bundle 'hdima/python-syntax'
VAMActivate newlisp
Bundle 'mustache/vim-mustache-handlebars'
" This is the same as vim's markdown support, plus it covers *.md
Bundle 'tpope/vim-markdown'
Bundle 'haproxy'
Bundle 'fatih/vim-go'
Bundle 'derekwyatt/vim-scala'


" NIX NIX NIX
VAMActivate vim-addon-nix

"Bundle 'VimClojure'
Bundle 'guns/vim-clojure-static'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'guns/vim-clojure-highlight'
Bundle 'tpope/vim-leiningen'
VAMActivate projectionist
VAMActivate dispatch
VAMActivate fireplace

" woo haskell
VAMActivate indenthaskell
" This is cute but really obnoxious:
"Bundle 'frerich/unicode-haskell'

"Bundle 'insanum/votl'   " I should learn how to use this
VAMActivate virtualenv
VAMActivate afterimage
" This isn't as cool as I thought it would be:
"Bundle 'UpdateDNSSerial'
Bundle 'Puppet_Syntax_Highlighting'
Bundle 'airblade/vim-gitgutter'

VAMActivate YouCompleteMe
VAMActivate AnsiEsc

" Smart selection of the closest text object
VAMActivate wildfire
VAMActivate vim-airline

filetype plugin indent on
