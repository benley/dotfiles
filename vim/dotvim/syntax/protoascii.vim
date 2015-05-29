" Borrowed from https://github.com/davidzchen/dotfiles/blob/master/vim/syntax/protoascii.vim
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

syn keyword pbasciiTodo       contained TODO FIXME XXX
syn cluster pbasciiCommentGrp contains=pbasciiTodo

syn match   pbasciiExtension  /\\[\S\+\\]/

syn keyword pbasciiBool       true false
syn match   pbasciiInt        /-\?\<\d\+\>/
syn match   pbasciiInt        /\<0[xX]\x+\>/
syn match   pbasciiFloat      /\<-\?\d*\(\.\d*\)\?/
syn region  pbasciiComment    start="#" skip="\\$" end="$" keepend contains=@pbCommentGrp
syn region  pbasciiString     start=/"/ skip=/\\"/ end=/"/

if version >= 508 || !exists("did_proto_syn_inits")
  if version < 508
    let did_proto_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink pbasciiTodo    Todo

  HiLink pbasciiInt     Number
  HiLink pbasciiFloat   Float
  HiLink pbasciiComment Comment
  HiLink pbasciiString  String
  HiLink pbasciiBool    Boolean

  HiLink pbasciiExtension Keyword

  delcommand HiLink
endif

let b:current_syntax = "protoascii"
