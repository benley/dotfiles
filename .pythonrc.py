# To use this, put this in your .bashrc or other shell login file:
# export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# This makes tab-completion work in interactive python interpreters, as long
# as they are compiled with gnu readline support. If not, python will still
# work, just without tab completion.
try:
  import readline
except ImportError:
  print "Module readline not available."
else:
  import rlcompleter
  import os
  sysname = os.uname()[0]
  print sysname
  if sysname == 'Linux':
    readline.parse_and_bind("tab: complete")
  elif sysname == 'Darwin':
    readline.parse_and_bind("bind ^i rl_complete")
  del sysname

# Restore our command-line history, and save it when Python exits.
# Similar to how bash does it with .bash_history.
import atexit
import os.path
history_path = os.path.expanduser('~/.python_history')
if os.path.isfile(history_path):
   readline.read_history_file(history_path)
atexit.register(lambda x=history_path: readline.write_history_file(x))
del history_path
