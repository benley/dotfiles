# -*- sh-shell: bash -*-
alias getenv='source "$HOME"/.ssh/.getenv'
alias grep='grep --color'
alias wat='type -a'

ssh-keyrm() {
  if [[ $# -eq 1 ]]; then
    ssh-keygen -f "$HOME/.ssh/known_hosts" -R "$1"
  else
    echo "USAGE: ssh-keyrm <hostname>" >&2
    return 1
  fi
}

amqping() {
  [[ $# -eq 1 && $# -le 2 ]] || { echo "USAGE: amqping <hostname> [port]" >&2; return 1; }
  cmd="echo -e 'AMQP\x00\x00\x09\x01' | nc -w 1 '$1' '${2:-5672}' | cat -t"
  echo "$cmd" >&2
  eval $cmd
  echo
}

alias e='emacseditor'
