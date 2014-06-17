# To use this, put this in your .bashrc or other shell login file:
# export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# This makes tab-completion work in interactive python interpreters, as long
# as they are compiled with gnu readline support. If not, python will still
# work, just without tab completion.
try:
    import readline
except ImportError:
    print("This python lacks readline, so history and tab completion won't work.")
else:
    import rlcompleter
    import os
    sysname = os.uname()[0]
    if sysname == 'Linux':
        readline.parse_and_bind("tab: complete")
    elif sysname == 'Darwin':
        # brew python:
        readline.parse_and_bind("tab: complete")
        # mac python:
        # readline.parse_and_bind("bind ^i rl_complete")
        # I think macos doesn't use gnu readline, but rather BSD libedit, which
        # acts like it but isn't quite the same. Brew uses actual gnu readline
        # when building python.
    del sysname

# Restore our command-line history, and save it when Python exits.
# Similar to how bash does it with .bash_history.
if readline:
    import atexit
    import os.path
    history_path = os.path.expanduser('~/.python_history')
    if os.path.isfile(history_path):
        readline.read_history_file(history_path)
    atexit.register(lambda x=history_path: readline.write_history_file(x))
    del history_path
