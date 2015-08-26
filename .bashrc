#!bash
# .bashrc
umask 0022

OS=$(uname)
# PLATFORM=$(uname -sm | tr " " "-")
export EDITOR=vim

prefixpath() {
  [[ -d "$1"/ ]] && PATH="$1${PATH:+:$PATH}"|| return 1
}

addpath() {
  [[ -d "$1"/ ]] && PATH="${PATH:+$PATH:$1}" || return 1
}

dopath() {
  prefixpath /usr/local/bin
  prefixpath "$HOME/go/bin"
  prefixpath "$HOME/.local/bin"  # Python user installs
  prefixpath "$HOME/Library/Haskell/bin"
  #prefixpath "$HOME/.cabal/bin"
  #prefixpath /usr/local/opt/ruby/bin
  prefixpath "$HOME/bin"
  # addpath $HOME/arcanist/arcanist/bin
  # addpath $HOME/opt/node/bin
  # addpath /usr/local/share/npm/bin
  # addpath /usr/local/heroku/bin
  # addpath $HOME/.gem/ruby/1.9.1/bin
  # addpath $HOME/.local/share/Steam/debian_bin
  # addpath $HOME/Dropbox/bin{,/$PLATFORM}
  addpath /usr/local/sbin

  [[ -e /etc/arch-release && -e /usr/bin/ruby ]] && \
    PATH+="$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
}; dopath

# This is unnecessary: see bash manpage's INVOCATION section.
# Bash will source /etc/bash.bashrc by itself.
#if [[ -f /etc/bashrc ]]; then
#  . /etc/bashrc
#fi

alias getenv='source "$HOME"/.ssh/.getenv'

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

# Enable extended ** glob expansion
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Fancy timestamps in .bash_history woooooo
export HISTTIMEFORMAT='%Y-%m-%d %T '

case "$OS" in
  "Linux")
    alias ls='ls --color=auto'
    eval "$(dircolors ~/.dircolors)"
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
  alias grep='grep --color'
fi


loadcompletion() {
  local spot cmpl _completion_on=0
  local completion_dirs=(
    "$HOME/.nix-profile/share/bash-completion"
    /opt/local/etc
    /opt/local/share/bash-completion
    "${brew_prefix}/etc"
    "${brew_prefix}/share/bash-completion"
    /etc
    )

  for spot in "${completion_dirs[@]}"; do
    if [[ -e ${spot}/bash_completion ]]; then
      source "${spot}/bash_completion"
      _completion_on=1
      break
    fi
  done

  if ((_completion_on)); then
    for cmpl in $HOME/.bash_completion.d/* \
                $HOME/.nix-profile/etc/bash_completion.d/*; do
      source "$cmpl"
    done

    if [[ $(type -t _npm_completion) != 'function' && $(type -t npm) != '' ]]; then
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

# less: extended status prompt, display ANSI colors, case-insensitive search.
export LESS="-M -R -i"

export PYTHONSTARTUP="${HOME}/.pythonrc.py"

# MOAR HISTORY
export HISTSIZE=9999
export HISTFILESIZE=9999

# Debian dev stuff
export DEBEMAIL='benley@gmail.com'
export DEBFULLNAME='Benjamin Staffin'
alias lintian="lintian --color=auto"

if [[ -d "$HOME/.nix-profile/etc/profile.d" ]]; then
  for file in $HOME/.nix-profile/etc/profile.d/*; do
    source "$file"
  done
fi

if [[ -d "$HOME/.bashrc.d" ]]; then
  for file in $HOME/.bashrc.d/*; do
    source "$file"
  done
fi

[[ "$OS" == "Darwin" && -e '/usr/local/bin/ctags' ]] && \
    alias ctags='/usr/local/bin/ctags'

# set variable identifying the chroot you work in (used in the prompt below)
if [[ -z "${debian_chroot:-}" && -r /etc/debian_chroot ]]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_DESCRIBE_STYLE=branch

myPromptCmd() {
  local venvprompt prompt1 prompt2 prompt3
  venvprompt=""
  if [[ "$VIRTUAL_ENV" =~ (\/([^/]+))+ ]]; then
    export venvprompt="─\[\033[01;36m\]${BASH_REMATCH[-1]}\[\033[00m\]"
  fi

  prompt1="┌─( \[\033[01;32m\]\u\[\033[00m\]@\h )─( \[\033[01;34m\]\w\[\033[00m\] )"
  prompt2="\n└─${debian_chroot:+(\[\033[01;35m\]$debian_chroot\[\033[00m\])-}\[\033[00m\]($?)${venvprompt}─> \$ "
  prompt3="─( %s )"

  case $TERM in
    xterm*)
      # Set the title bar to include the current directory.
      prompt1="\[\033]0;\u@\h: \w\007\]${prompt1}"
      ;;
  esac

  __git_ps1 "${prompt1}" "${prompt2}" "${prompt3}"
}

PROMPT_COMMAND="myPromptCmd"

[[ "$COLORTERM" == 'gnome-terminal' ]] && export TERM='xterm-256color'

# Handled via nix
#[[ -e /usr/local/bin/virtualenvwrapper.sh ]] && source /usr/local/bin/virtualenvwrapper.sh

# for virtualenvwrapper
export PROJECT_HOME="$HOME/projects"

# I can pipe things to less when I want an interactive pager, thank you very much.
export NIX_PAGER=

if [[ -e "$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt" ]]; then
  export GIT_SSL_CAINFO=$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt
fi

return 0
