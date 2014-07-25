set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-jdaddy'
Bundle 'benmills/vimux'
"Bundle 'fholgado/minibufexpl.vim'
Bundle 'vim-scripts/genutils'
"Bundle 'instant-markdown.vim'

" Color schemes:
Bundle 'Ambient-Color-Scheme'
"Bundle 'clarity.vim'
"Bundle 'oceandeep'
"Bundle 'Cthulhian'
Bundle 'vim-scripts/Zenburn'
Bundle 'altercation/vim-colors-solarized'

"Bundle 'Conque-Shell'
Bundle 'rosenfeld/conque-term'
Bundle 'pythoncomplete'
Bundle 'rogerz/vim-json'
Bundle 'scrooloose/nerdtree'
Bundle 'Tagbar'

Bundle 'taglist.vim'
Bundle 'Syntastic'

" I think this does some messy stuff that interferes with things other than
" Python. Grr.
"Bundle 'python_ifold'

Bundle 'SuperTab-continued.'
Bundle 'RDoc'
"Bundle 'confluencewiki.vim'
Bundle 'gitv'
Bundle 'pydoc.vim'

" Syntax highlighting:
Bundle 'pantsbuild/vim-pants'
Bundle 'ekalinin/Dockerfile.vim'
Bundle 'nginx.vim'
"Bundle 'fish.vim'
Bundle 'vim-coffee-script'
" http://www.vim.org/scripts/script.php?script_id=790 :
Bundle 'hdima/python-syntax'
Bundle 'newlisp'
Bundle 'mustache/vim-mustache-handlebars'

"Bundle 'VimClojure'
Bundle 'guns/vim-clojure-static'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'guns/vim-clojure-highlight'
Bundle 'tpope/vim-leiningen'
Bundle 'tpope/vim-projectionist'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-fireplace'

" woo haskell
Bundle 'indenthaskell.vim'
" This is cute but really obnoxious:
"Bundle 'frerich/unicode-haskell'

"Bundle 'insanum/votl'   " I should learn how to use this
Bundle 'virtualenv.vim'
Bundle 'afterimage.vim'
" This isn't as cool as I thought it would be:
"Bundle 'UpdateDNSSerial'
Bundle 'Puppet-Syntax-Highlighting'
Bundle 'airblade/vim-gitgutter'

Bundle 'Valloric/YouCompleteMe'
Bundle 'AnsiEsc.vim'

" Smart selection of the closest text object
Bundle 'gcmt/wildfire.vim'
Bundle 'bling/vim-airline'

filetype plugin indent on
