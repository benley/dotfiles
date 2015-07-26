" Vim syntax file
" Language:     Tumblr HTML Theme
" Maintainer:   Liam Cooke <liamcooke@gmail.com>
" URL:          http://github.com/inky/tumblr/tree/master/vim/
" Last Change:  2010 Mar 10

runtime! syntax/html.vim
unlet b:current_syntax

syntax region cssDefinition transparent matchgroup=cssBraces start='{' end='}' contains=css.*Attr,css.*Prop,cssComment,cssValue.*,cssColor,cssURL,cssImportant,cssError,cssStringQ,cssStringQQ,cssFunction,cssUnicodeEscape,tumblrTag

syntax match tumblrBlock '{/\?block:[A-Za-z0-9-]\+}' containedin=ALL
syntax match tumblrTag '{[A-Za-z0-9-:]\+}' containedin=ALL
syntax match tumblrTag '{[A-Za-z0-9-:]\+\( [A-Za-z0-9]\+="[A-Za-z0-9]\+"\)*}' containedin=ALL

hi def link tumblrBlock Label
hi def link tumblrTag Identifier

let b:current_syntax = "tumblr"
