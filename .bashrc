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
  prefixpath /usr/local/bin
  local addpaths=(
      $HOME/bin
      $HOME/arcanist/arcanist/bin
      $HOME/p/{depot_tools,android-ndk,android-sdk/{platform-,}tools}
      $HOME/Dropbox/bin{,/$PLATFORM}
      $HOME/.local/share/Steam/debian_bin
      /{usr,opt}/local/sbin
      $HOME/opt/node/bin
      /usr/local/share/npm/bin)
  for dir in ${addpaths[@]}; do
    [[ -d "$dir"/ ]] && PATH+=":${dir}"
  done

  [[ -e /etc/arch-release && -e /usr/bin/ruby ]] && \
    PATH+="$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
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
  for spot in {/opt/local,"${brew_prefix}",}/{etc,share/bash-completion}/bash_completion; do
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

export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# MOAR HISTORY
export HISTSIZE=9999
export HISTFILESIZE=9999

# Debian dev stuff
export DEBEMAIL='ben@cloudscaling.com'
export DEBFULLNAME='Benjamin Staffin'
alias lintian="lintian --color=auto"

if [[ -d "$HOME/.bashrc.d" ]]; then
  for file in $(find "$HOME/.bashrc.d" -type f -or -type l); do
    source "$file"
  done
fi

# Ruby?
[[ -e /usr/local/bin/ruby19 ]] && alias ruby=/usr/local/bin/ruby19
[[ -e /usr/local/bin/irb19 ]] && alias irb=/usr/local/bin/irb19
[[ -e /usr/local/bin/gem19 ]] && alias gem=/usr/local/bin/gem19

alias gerrit="ssh ben@pd.cloudscaling.com -p 29418 -- gerrit \$@"

[[ "$OS" == "Darwin" && -e '/usr/local/bin/ctags' ]] && \
    alias ctags='/usr/local/bin/ctags'

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_DESCRIBE_STYLE=branch

prompt1='┌─( \[\033[01;32m\]\u\[\033[00m\]@\h )─( \[\033[01;34m\]\w\[\033[00m\] )'
prompt2='\n└─${debian_chroot:+(\[\033[01;35m\]$debian_chroot\[\033[00m\])-}\[\033[00m\]($?)─> \$ '
prompt3='─( %s )'

# Cloudscaling logo:
#PS1='\[\033[1;30;47m\]cloud\[\033[0;31;47m\]scaling\[\033[;91m\] OCS 2.6.1\[\033[00m\]\n[\u@\h] \W \$ '

case $TERM in
  xterm*)
    # Set the title bar to include the current directory.
    prompt1="\[\033]0;\u@\h: \w\007\]${prompt1}"
    ;;
esac

PROMPT_COMMAND="__git_ps1 '${prompt1}' '${prompt2}' '${prompt3}'"
unset prompt1 prompt2 prompt3

[[ "$COLORTERM" == 'gnome-terminal' ]] && export TERM='xterm-256color'
[[ -e /usr/local/bin/virtualenvwrapper.sh ]] && source /usr/local/bin/virtualenvwrapper.sh

# for virtualenvwrapper
export PROJECT_HOME="$HOME/projects"

return 0
