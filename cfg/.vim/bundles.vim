" nix wrapper loads VAM now:
"
" set nocompatible | filetype indent plugin on | syn on
"
" fun! SetupVAM()
"   let c = get(g:, 'vim_addon_manager', {})
"   let g:vim_addon_manager = c
"   let c.plugin_root_dir = expand('$HOME', 1) . '/.vim/vim-addons'
"   " most used options you may want to use:
"   let c.log_to_buf = 1
"   let c.auto_install = 1
"   let &rtp.=(empty(&rtp)?'':',').c.plugin_root_dir.'/vim-addon-manager'
"   if !isdirectory(c.plugin_root_dir.'/vim-addon-manager/autoload')
"     execute '!git clone --depth=1 git://github.com/MarcWeber/vim-addon-manager '
"         \       shellescape(c.plugin_root_dir.'/vim-addon-manager', 1)
"   endif
"
"   " This provides the VAMActivate command. You could be passing plugin names, too
"   call vam#ActivateAddons([], {})
" endfun
" call SetupVAM()
"call vundle#rc()

""" Misc
VAMActivate afterimage
VAMActivate AnsiEsc
"VAMActivate eunuch  "nix
"VAMActivate fugitive  "nix
VAMActivate genutils
VAMActivate github:depuracao/vim-rdoc
"VAMActivate github:insanum/votl   " I should learn how to use this
VAMActivate github:rogerz/vim-json
VAMActivate github:rosenfeld/conque-term
VAMActivate gitv
VAMActivate jdaddy
"VAMActivate Syntastic  "nix
"VAMActivate Supertab  "nix
"VAMActivate Tagbar  "nix
"VAMActivate taglist  "nix
"VAMActivate The_NERD_tree  "nix
VAMActivate vimux
"VAMActivate github:vim-airline/vim-airline  "nix
"VAMActivate github:vim-airline/vim-airline-themes  "nix
"VAMActivate vim-gitgutter  "nix
"VAMActivate vim-vagrant  " provides :Vagrant
VAMActivate wildfire  " Smart selection of the nearest text object
"VAMActivate YouCompleteMe "nix
"VAMActivate github:rdnetto/YCM-Generator  " Disabled until I use YCM again
"VAMActivate github:jeetsukumaran/vim-buffergator  "nix
"VAMActivate github:godlygeek/tabular  "nix
"""

" TODO: figure this out.
"VAMActivate github:syngan/vim-vimlint
"VAMActivate github:ynkdir/vim-vimlparser

""" Color schemes
VAMActivate Ambient_Color_Scheme
VAMActivate clarity
VAMActivate Cthulhian
VAMActivate darktango
"VAMActivate github:chriskempson/base16-vim  "nix
VAMActivate gruvbox
VAMActivate Lucius
"VAMActivate molokai  "nix
VAMActivate monokai
VAMActivate oceandeep
VAMActivate oceanlight
"VAMActivate Solarized  "nix
"VAMActivate Zenburn  "nix
"""

""" Misc syntax highlighting:
"VAMActivate fish
"VAMActivate github:ekalinin/Dockerfile.vim  "nix
VAMActivate github:hylang/vim-hy
VAMActivate github:manicmaniac/coconut.vim
VAMActivate github:kevints/vim-aurora-syntax
VAMActivate github:kylef/apiblueprint.vim
VAMActivate github:mustache/vim-mustache-handlebars
VAMActivate github:pangloss/vim-javascript
"VAMActivate github:pantsbuild/vim-pants
"VAMActivate github:davidzchen/vim-bazel
VAMActivate github:google/vim-ft-bzl
VAMActivate github:tpope/vim-markdown  " Basically just adds *.md detection
VAMActivate haproxy
VAMActivate newlisp
VAMActivate nginx
"VAMActivate Puppet_Syntax_Highlighting
"VAMActivate github:rodjek/vim-puppet
VAMActivate github:voxpupuli/vim-puppet
"VAMActivate github:LnL7/vim-nix  "nix
"VAMActivate vim-coffee-script  "nix
"VAMActivate vim-go  "nix
VAMActivate vim-scala
VAMActivate github:solarnz/thrift.vim
"VAMActivate github:rust-lang/rust.vim  "nix
VAMActivate github:cespare/vim-toml
"VAMActivate github:google/vim-jsonnet  "nix
VAMActivate github:Glench/Vim-Jinja2-Syntax
VAMActivate github:rix0rrr/vim-gcl
"VAMActivate github:bracki/vim-prometheus
VAMActivate github:skreuzer/vim-prometheus
VAMActivate github:jubalfh/vim-ldapschema
VAMActivate github:robbles/logstash.vim
VAMActivate github:keith/swift.vim
VAMActivate github:chikamichi/mediawiki.vim
"""

VAMActivate github:aquach/vim-mediawiki-editor

""" Clojure
VAMActivate dispatch
VAMActivate fireplace
VAMActivate github:guns/vim-clojure-highlight
VAMActivate github:guns/vim-clojure-static
"VAMActivate github:kien/rainbow_parentheses.vim  "nix
VAMActivate github:tpope/vim-leiningen
VAMActivate projectionist
"""

""" Haskell
"VAMActivate ghcmod  "nix
"VAMActivate github:bitc/lushtags  " broken and/or deprecated?
"VAMActivate github:eagletmt/neco-ghc  "nix
"VAMActivate github:lukerandall/haskellmode-vim
"VAMActivate github:dag/vim2hs  "nix
"VAMActivate indenthaskell
"""

""" Python
"VAMActivate github:hdima/python-syntax  " seems to break pyrex/cython syntax?
"VAMActivate pythoncomplete  " this comes with vim now
"VAMActivate pydoc
"VAMActivate python_ifold  " Interferes with things other than Python. Grr.
"VAMActivate virtualenv  "nix
"""

" For mouse clicks
"VAMActivate utl
