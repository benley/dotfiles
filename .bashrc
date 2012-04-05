# .bashrc

OS=$(uname)

# This is unnecessary: see bash manpage's INVOCATION section.
# Bash will source /etc/bash.bashrc by itself.
#if [[ -f /etc/bashrc ]]; then
#  . /etc/bashrc
#fi

source $HOME/bin/benlib.sh
alias c='kssh catbus'

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

export EDITOR=vim
export PATH="$HOME/bin:$HOME/android-sdk/tools:${PATH}"
export DEBEMAIL='benley@zoiks.net'

# Fancy timestamps in .bash_history woooooo
export HISTTIMEFORMAT='%Y-%m-%d %T '

case "${OS}" in
  "Linux")
    export P4EDITOR="${EDITOR}"
    alias ls='ls --color=auto'
    eval $(dircolors ~/.dircolors)
    ;;
  "Darwin")
    export CLICOLOR="true"
    # Colors I picked out long ago or something?
    #export LSCOLORS="DeGxxxxxCx"
    # Solarized-like colors:
    export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
    export CLICOLOR=1
    ;;
esac

if grep --version|grep -q GNU; then
  export GREP_OPTIONS="--color"
fi

[[ -f /etc/bash_completion ]] && source /etc/bash_completion

case $TERM in
  xterm*)
    # Set the title bar to include the current directory.
    PS1="\[\033]0;\u@\h: \w\007\][\u@\h \W]\\$ "
    ;;
  *)
    PS1="[\u@\h \W]\\$ "
    ;;
esac

# This is nifty but messes up display of long history lines.
#export PS1='\[┌─\](\w)\[────\]\n\[└\][\h]\$ '

# fancy-ass manpage colors!
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export PYTHONSTARTUP="${HOME}/.pythonrc"

# MOAR HISTORY
export HISTSIZE=9999
export HISTFILESIZE=9999
