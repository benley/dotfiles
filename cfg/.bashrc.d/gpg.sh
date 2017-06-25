#bash

GPG_TTY=$(tty)
export GPG_TTY

if [[ -z $SSH_CLIENT || -z $SSH_AUTH_SOCK ]]; then
  unset SSH_AGENT_PID

  if [[ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]]; then
    SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
    export SSH_AUTH_SOCK
  fi

  gpg2 --card-status >/dev/null 2>&1 ||true
fi
