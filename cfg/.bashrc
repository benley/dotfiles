#!bash
# .bashrc
umask 0022

OS=$(uname)
export EDITOR=vim

PATH="$HOME/bin:$PATH"

# This is unnecessary: see bash manpage's INVOCATION section.
# Bash will source /etc/bash.bashrc by itself.
#if [[ -f /etc/bashrc ]]; then
#  . /etc/bashrc
#fi

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
#HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Enable extended ** glob expansion
shopt -s globstar

# make less friendlier for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Fancy timestamps in .bash_history woooooo
HISTTIMEFORMAT='%Y-%m-%d %T '

case "$OS" in
  "Linux")
    alias ls='ls --color=auto'
    eval "$(dircolors ~/.dircolors)"
    ;;
  "Darwin")
    alias ls='ls -F'
    brew_prefix=$(brew --prefix 2>/dev/null)
    # Colors I picked out long ago or something?
    #export LSCOLORS="DeGxxxxxCx"
    # Solarized-like colors:
    export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
    export CLICOLOR=1
    ;;
esac

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
                /usr/local/etc/bash_completion.d/* \
                $HOME/.nix-profile/etc/bash_completion.d/*; do
      [[ -e "$cmpl" ]] && source "$cmpl"
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
HISTSIZE=9999
HISTFILESIZE=9999

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
  declare -A colors
  colors=(
    [normal]='\[\033[0m\]'
    [black]='\[\033[0;30m\]'
    [red]='\[\033[0;31m\]'
    [green]='\[\033[0;32m\]'
    [yellow]='\[\033[0;33m\]'
    [blue]='\[\033[0;34m\]'
    [magenta]='\[\033[0;35m\]'
    [cyan]='\[\033[0;36m\]'
    [white]='\[\033[0;37m\]'
    [brightblack]='\[\033[0;38;5;8m\]'  # wat
    [brightred]='\[\033[0;38;5;9m\]'
    [brightgreen]='\[\033[0;38;5;10m\]'
    [brightyellow]='\[\033[0;38;5;11m\]'
    [brightblue]='\[\033[0;38;5;12m\]'
    [brightmagenta]='\[\033[0;38;5;13m\]'
    [brightcyan]='\[\033[0;38;5;14m\]'
    [brightwhite]='\[\033[0;38;5;15m\]'
  )

  local venvprompt venvname prompt1 prompt2 prompt3
  venvprompt=""
  if [[ "$VIRTUAL_ENV" =~ (\/([^/]+))+ ]]; then
    venvname=${BASH_REMATCH[-1]}
    venvprompt="─${colors[brightcyan]}${venvname}${colors[normal]}"
  fi

  prompt1="┌─( ${colors[brightgreen]}\u${colors[normal]}@\h "
  prompt1+=")─( ${colors[brightblue]}\w${colors[normal]} )"
  prompt2="\n└${debian_chroot:+─(${colors[brightmagenta]}${debian_chroot}${colors[normal]})}"
  prompt2+="─(\$?)"
  prompt2+="${venvprompt}─> \\$ "
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

# Handled via nix
#[[ -e /usr/local/bin/virtualenvwrapper.sh ]] && source /usr/local/bin/virtualenvwrapper.sh

# for virtualenvwrapper
export PROJECT_HOME="$HOME/projects"

# I can pipe things to less when I want an interactive pager, thank you very much.
export NIX_PAGER=

if [[ -e "$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt" ]]; then
  export GIT_SSL_CAINFO=$HOME/.nix-profile/etc/ssl/certs/ca-bundle.crt
fi

if ! [[ "$NIX_PATH" =~ dotfiles= ]]; then
  NIX_PATH="${NIX_PATH:+$NIX_PATH:}dotfiles=https://github.com/benley/dotfiles/archive/master.tar.gz"
fi

: # always end with $?=0
