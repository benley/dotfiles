" Attempts to make `gf` file jumping work for python imports.
python << EOF
import os
import sys
import vim
for p in sys.path:
  if os.path.isdir(p):
    vim.command(r"set path+=%s" % p.replace(" ", r"\ "))
EOF

call BenCode()
setlocal tags+=$HOME/.vim/tags/python27.tags
setlocal indentexpr=GetGooglePythonIndent(v:lnum)
setlocal shiftwidth=4
setlocal formatprg=autopep8\ -
setlocal omnifunc=pythoncomplete#Complete

" From http://google-styleguide.googlecode.com/svn/trunk/google_python_style.vim
" See also http://google-styleguide.googlecode.com/svn/trunk/pyguide.html
let s:maxoff = 50 " maximum number of lines to look backwards.
function GetGooglePythonIndent(lnum)
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

" This will double-indent nested and open-paren indents:
"let pyindent_nested_paren="&sw*2"
"let pyindent_open_paren="&sw*2"

" But if we're doing pep8 style (4-space indent) we just want 1x, which is
" conveniently the same value as a double-indent in google style, so:
let pyindent_nested_paren = 4
let pyindent_open_paren = 4

"" This helps syntastic work with virtualenv.
"if has("python") && !empty($VIRTUAL_ENV)
"  python << EOF
"import os
"import sys
"a = os.environ['VIRTUAL_ENV'] + '/bin/activate_this.py'
"execfile(a, dict(__file__ = a))
"if 'PYTHONPATH' not in os.environ:
"  os.environ['PYTHONPATH'] = ''
"  os.environ['PYTHONPATH'] += ":"+os.getcwd()
"  os.environ['PYTHONPATH'] += ":".join(sys.path)
"EOF
"endif
