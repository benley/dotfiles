
" Vim MouseClick
" see the README.md in the root of this project
"
"
" dev note:
"   http://vim.wikia.com/wiki/How_to_write_a_plugin
"   plugin/*.vim are standard and always loaded at startup.
"   autoload/*.vim is autoloaded lazily only when needed.
"
"   rerun this plugin:
"     :set runtimepath+=... " only if needed
"     :set verbose=1 " not sure
"     :runtime plugin/mouseclick.vim
"     
"   map keys:
"     http://stackoverflow.com/questions/3776117/vim-what-is-the-difference-between-the-remap-noremap-nnoremap-and-vnoremap-ma
"
"
" discussion:
"   https://groups.google.com/forum/#!topic/vim_mac/7uXkwq7kJ_4
"   http://superuser.com/questions/667821/macvim-make-urls-clickable

" It needs [utl](http://www.vim.org/scripts/script.php?script_id=293),
" not sure how to check for that.

if &compatible
  finish
endif

if exists("g:loaded_VimMouseClick") || &compatible
  "  finish
  "  no finish, I want to support reloading (or how should I do development
  "  otherwise)
endif

let g:loaded_VimMouseClick = 1
let s:save_cpo = &cpo
set cpo&vim

function! s:Open()
  let word = expand("<cWORD>")
  exec ":Utl openLink " . word
  " sil exe "! open " . word
endfunction


" http://stackoverflow.com/questions/10139972/vim-hasmacunix-or-hasmac-do-not-work
let os=substitute(system('uname'), '\n', '', '')
if os == 'Darwin' || os == 'Mac'
  " for macosx macvim:
  sil exec "! defaults write org.vim.MacVim MMTranslateCtrlClick 0"

  if !exists("g:utl_cfg_hdl_scm_http_system")
    let g:utl_cfg_hdl_scm_http_system = "silent !open '%u#%f'"
  endif
endif


map <C-LeftMouse> <LeftMouse> :call <SID>Open()<CR>
imap <C-LeftMouse> <LeftMouse><ESC> :call <SID>Open()<CR>i

let &cpo = s:save_cpo



" vim: fdm=marker:noet:ts=4:sw=4:sts=4
