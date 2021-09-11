# -*- sh-shell: bash -*-
# shellcheck shell=bash

__prompt_command() {
  e=$?

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

  local venvprompt="" venvname

  if [[ $VIRTUAL_ENV =~ (\/([^/]+))+ ]]; then
    venvname=${BASH_REMATCH[-1]}
    venvprompt="─${colors[brightcyan]}${venvname}${colors[normal]}"
  fi

  local before="" after=""

  case $TERM in
    xterm*|alacritty)
      # Set the title bar to include the current directory.
      before+="\[\033]0;\u@\h: \w\007\]"
      ;;
  esac

  # first line:  right-aligned HH:MM:SS, then put the cursor back at column 0
  before+="$(printf "%$((COLUMNS - 8))s")\t\r"
  # ... then left-aligned half of the first line
  before+="┌─( ${colors[brightgreen]}\u${colors[normal]}@\h "
  before+=")─( ${colors[brightblue]}\w${colors[normal]} )"

  # the part that __git__ps1 formats into:
  local git_status_fmt="─( %s )"

  # End of the first line: show my current kubernetes context, if any
  # if [[ $PWD =~ ~/[pm].* ]] && ...
  if kube_context=$(kubectl config current-context 2>/dev/null); then
    after="─(${colors[blue]}☸️${colors[normal]} ${kube_context})"
  fi

  # start of second line: If I'm in a debian chroot, highlight that
  after+="\n└${debian_chroot:+─(${colors[brightmagenta]}${debian_chroot}${colors[normal]})}"
  # also nix-shell
  after+="${IN_NIX_SHELL:+─(${colors[brightblue]}nix-shell${colors[normal]})}"

  # Numeric exit status of the last command, if non-zero
  case $e in
    0) : ;;
    # 0) after+="($e)" ;;
    *) after+="${colors[red]}($e)${colors[normal]}" ;;
  esac
  # Current virtualenv name (if any), then the final $
  after+="${venvprompt}─> \\$ "

  # Directory tracking for emacs-libvterm (needs to be at the end, after all visible parts)
  if [[ $INSIDE_EMACS == "vterm" ]]; then
    after+="\[\$(vterm_prompt_end)\]"
  fi

  GIT_PS1_SHOWDIRTYSTATE=1 GIT_PS1_SHOWCOLORHINTS=1 GIT_PS1_DESCRIBE_STYLE=branch \
    __git_ps1 "$before" "$after" "$git_status_fmt"
}
PROMPT_COMMAND="${PROMPT_COMMAND:+${PROMPT_COMMAND%;};}__prompt_command"
