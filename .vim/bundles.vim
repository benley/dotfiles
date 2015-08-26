set nocompatible | filetype indent plugin on | syn on

fun! SetupVAM()
  let c = get(g:, 'vim_addon_manager', {})
  let g:vim_addon_manager = c
  let c.plugin_root_dir = expand('$HOME', 1) . '/.vim/vim-addons'
  " most used options you may want to use:
  let c.log_to_buf = 1
  let c.auto_install = 1
  let &rtp.=(empty(&rtp)?'':',').c.plugin_root_dir.'/vim-addon-manager'
  if !isdirectory(c.plugin_root_dir.'/vim-addon-manager/autoload')
    execute '!git clone --depth=1 git://github.com/MarcWeber/vim-addon-manager '
        \       shellescape(c.plugin_root_dir.'/vim-addon-manager', 1)
  endif

  " This provides the VAMActivate command. You could be passing plugin names, too
  call vam#ActivateAddons([], {})
endfun
call SetupVAM()

"call vundle#rc()

""" Misc
VAMActivate afterimage
VAMActivate AnsiEsc
VAMActivate eunuch
VAMActivate fugitive
VAMActivate genutils
VAMActivate github:depuracao/vim-rdoc
"VAMActivate github:insanum/votl   " I should learn how to use this
VAMActivate github:rogerz/vim-json
VAMActivate github:rosenfeld/conque-term
VAMActivate gitv
VAMActivate jdaddy
VAMActivate Syntastic
VAMActivate Supertab
VAMActivate Tagbar
VAMActivate taglist
VAMActivate The_NERD_tree
VAMActivate vimux
VAMActivate vim-airline
VAMActivate vim-gitgutter
VAMActivate vim-vagrant            " provides :Vagrant
VAMActivate wildfire               " Smart selection of the nearest text object
VAMActivate YouCompleteMe
"""

" TODO: figure this out.
"VAMActivate github:syngan/vim-vimlint
"VAMActivate github:ynkdir/vim-vimlparser

""" Color schemes
VAMActivate Ambient_Color_Scheme
VAMActivate clarity
VAMActivate Cthulhian
VAMActivate darktango
VAMActivate github:chriskempson/base16-vim
VAMActivate gruvbox
VAMActivate Lucius
VAMActivate molokai
VAMActivate monokai
VAMActivate oceandeep
VAMActivate oceanlight
VAMActivate Solarized
VAMActivate Zenburn
"""

""" Misc syntax highlighting:
"VAMActivate fish
VAMActivate github:ekalinin/Dockerfile.vim
VAMActivate github:hylang/vim-hy
VAMActivate github:kevints/vim-aurora-syntax
VAMActivate github:kylef/apiblueprint.vim
VAMActivate github:mustache/vim-mustache-handlebars
VAMActivate github:pangloss/vim-javascript
VAMActivate github:pantsbuild/vim-pants
VAMActivate github:tpope/vim-markdown  " Basically just adds *.md detection
VAMActivate haproxy
VAMActivate newlisp
VAMActivate nginx
VAMActivate Puppet_Syntax_Highlighting
VAMActivate vim-addon-nix
VAMActivate vim-coffee-script
VAMActivate vim-go
VAMActivate vim-scala
VAMActivate github:solarnz/thrift.vim
"""

""" Clojure
VAMActivate dispatch
VAMActivate fireplace
VAMActivate github:guns/vim-clojure-highlight
VAMActivate github:guns/vim-clojure-static
VAMActivate github:kien/rainbow_parentheses.vim
VAMActivate github:tpope/vim-leiningen
VAMActivate projectionist
"""

""" Haskell
VAMActivate ghcmod
"VAMActivate github:bitc/lushtags  " broken and/or deprecated?
VAMActivate github:eagletmt/neco-ghc
VAMActivate github:lukerandall/haskellmode-vim
VAMActivate indenthaskell
"""

""" Python
"VAMActivate github:hdima/python-syntax  " seems to break pyrex/cython syntax?
VAMActivate pythoncomplete
"VAMActivate pydoc
"VAMActivate python_ifold  " Interferes with things other than Python. Grr.
VAMActivate virtualenv
"""

filetype plugin indent on
