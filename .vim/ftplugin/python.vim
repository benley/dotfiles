

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
