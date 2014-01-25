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
Bundle 'vim-json-bundle'
Bundle 'scrooloose/nerdtree'
Bundle 'Tagbar'
Bundle 'taglist.vim'
Bundle 'Syntastic'
Bundle 'python_ifold'
Bundle 'SuperTab-continued.'
Bundle 'RDoc'
"Bundle 'confluencewiki.vim'
Bundle 'gitv'
Bundle 'pydoc.vim'

" Syntax highlighting:
Bundle 'ekalinin/Dockerfile.vim'
Bundle 'nginx.vim'
"Bundle 'fish.vim'
Bundle 'vim-coffee-script'
" http://www.vim.org/scripts/script.php?script_id=790 :
Bundle 'hdima/python-syntax'
Bundle 'newlisp'

" woo haskell
Bundle 'indenthaskell.vim'
" This is cute but really obnoxious:
"Bundle 'frerich/unicode-haskell'

"Bundle 'insanum/votl'   " I should learn how to use this
Bundle 'virtualenv.vim'
Bundle 'afterimage.vim'
" This isn't as cool as I thought it would be:
"Bundle 'UpdateDNSSerial'
Bundle 'Valloric/YouCompleteMe'
Bundle 'Puppet-Syntax-Highlighting'

filetype plugin indent on
