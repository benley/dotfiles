alias getenv='source "$HOME"/.ssh/.getenv'
alias grep='grep --color'

ssh-keyrm() {
  if [[ $# -eq 1 ]]; then
    ssh-keygen -f "$HOME/.ssh/known_hosts" -R "$1"
  else
    echo "USAGE: ssh-keyrm <hostname>" >&2
    return 1
  fi
}
