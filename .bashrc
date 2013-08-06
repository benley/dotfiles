# .bashrc

umask 0022

OS=$(uname)
PLATFORM=$(uname -sm | tr " " "-")
export EDITOR=vim

# In most places I'll keep the system's timezone. Makes reading logfiles easier.
[[ "$HOSTNAME" == 'osric' ]] && export TZ='America/Los_Angeles'

prefixpath() {
  [[ -d "$1"/ ]] && PATH="$1:$PATH" || return 1
}

dopath() {
  prefixpath "$HOME/Library/Haskell/bin"
  prefixpath "$HOME/.cabal/bin"
  prefixpath /usr/local/opt/ruby/bin
  local addpaths=(
      $HOME/bin
      $HOME/arcanist/arcanist/bin
      $HOME/p/{depot_tools,android-ndk,android-sdk/{platform-,}tools}
      $HOME/Dropbox/bin/{,$PLATFORM}
      $HOME/.local/share/Steam/debian_bin
      /{usr,opt}/local/{bin,sbin}
      $HOME/opt/node/bin
      /usr/local/share/npm/bin)
  for dir in ${addpaths[@]}; do
    [[ -d "$dir"/ ]] && PATH+=":${dir}"
  done
}; dopath

# This is unnecessary: see bash manpage's INVOCATION section.
# Bash will source /etc/bash.bashrc by itself.
#if [[ -f /etc/bashrc ]]; then
#  . /etc/bashrc
#fi

source $HOME/bin/benlib.sh
alias getenv='source "$HOME"/.ssh/.getenv'
alias mz='mosh zoiks.net -- $@'

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
#export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
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

# Fancy timestamps in .bash_history woooooo
export HISTTIMEFORMAT='%Y-%m-%d %T '

case "$OS" in
  "Linux")
    alias ls='ls --color=auto'
    eval $(dircolors ~/.dircolors)
    ;;
  "Darwin")
    brew_prefix=$(brew --prefix 2>/dev/null)
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


loadcompletion() {
  local spot cmpl _completion_on=0
  for spot in {/opt/local,$brew_prefix,}/etc/bash_completion; do
    if [[ -e $spot ]]; then
      source $spot
      _completion_on=1
      break
    fi
  done

  if ((_completion_on)); then
    for cmpl in $HOME/.bash_completion.d/*; do
      source $cmpl
    done

    if [[ $(type -t _npm_completion) != 'function' && -x $(which npm) ]]; then
      npm completion > "$HOME/.bash_completion.d/npm_completion.sh"
      source "${HOME}/.bash_completion.d/npm_completion.sh"
    fi
  fi
}; loadcompletion

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

# fancy-ass manpage colors! (classic)
#export LESS_TERMCAP_mb=$'\E[01;31m'
#export LESS_TERMCAP_md=$'\E[01;31m'
#export LESS_TERMCAP_me=$'\E[0m'
#export LESS_TERMCAP_se=$'\E[0m'
#export LESS_TERMCAP_so=$'\E[01;44;33m'
#export LESS_TERMCAP_ue=$'\E[0m'
#export LESS_TERMCAP_us=$'\E[01;32m'

# Less colors, solarized.
export LESS_TERMCAP_mb=$'\E[5m'           # begin blinking
export LESS_TERMCAP_md=$'\E[0;33m'        # begin bold
#export LESS_TERMCAP_md=$'\E[01;38;5;74m' # begin bold  (light blue)
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[1;30;43m'     # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[4;32m'        # begin underline

# Have less use the extended status prompt, and display ANSI colors.
export LESS="-M -R"

# I like shiny things.
[[ -x $(which colorgcc) ]] && export CC=colorgcc
[[ -x $(which colormake) ]] && alias make=colormake

export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# MOAR HISTORY
export HISTSIZE=9999
export HISTFILESIZE=9999

# Debian dev stuff
export DEBEMAIL='ben@cloudscaling.com'
export DEBFULLNAME='Benjamin Staffin'
alias dquilt="quilt --quiltrc=${HOME}/.quiltrc-dpkg"
alias lintian="lintian --color=auto"

rcfiles="$HOME"/.bashrc.d/*
if [[ "$rcfiles" != "$HOME/.bashrc.d/*" ]]; then
  for file in $rcfiles; do
    source "$file"
  done
fi; unset rcfiles

# Ruby?
[[ -e /usr/local/bin/ruby19 ]] && alias ruby=/usr/local/bin/ruby19
[[ -e /usr/local/bin/irb19 ]] && alias irb=/usr/local/bin/irb19
[[ -e /usr/local/bin/gem19 ]] && alias gem=/usr/local/bin/gem19

if [[ $- =~ i && -x $(which keychain) ]]; then
  KEYS=".ssh/id_dsa .ssh/id_rsa .ssh/id_ben_cs .ssh/id_cloudscaling"
  eval $(keychain --inherit any --eval --ignore-missing --nogui --quiet --quick ${KEYS})
fi

alias gerrit="ssh ben@pd.cloudscaling.com -p 29418 -- gerrit \$@"
