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
    readline.parse_and_bind("tab: complete")
    if sysname == 'Darwin':
        # Really not sure if this is the right thing to check here
        if readline._READLINE_VERSION <= 1026:
            readline.parse_and_bind("bind ^i rl_complete")
        # I think macos doesn't use gnu readline, but rather BSD libedit, which
        # acts like it but isn't quite the same. Brew uses actual gnu readline
        # when building python.

    # Restore repl history, and save on exit.
    # Similar to how bash does it with .bash_history.
    import atexit
    import os.path
    history_path = os.path.expanduser('~/.python_history')
    if os.path.isfile(history_path):
        try:
            readline.read_history_file(history_path)
        except IOError as err:
            print("Failed to load history %s: %s" % (history_path, err))
    atexit.register(lambda x=history_path: readline.write_history_file(x))

    # Clear out globals before the repl session starts
    del atexit
    del history_path
    # del readline  # need this for the atexit lambda to work
    del rlcompleter
    del os
    del sysname
