#!/bin/bash

function krun() {
  if kinit -R || kinit benley@UNIX.CORP.GOOGLE.COM; then
    $@
  else
    echo "krun: Kerberos failed."
    return 1
  fi
}

function kssh() {
  krun ssh $@
}

function irc() {
  local pre=""
  local msg=""
  case $1 in
    "-p"|"--prompt")
      pre="waitonfail -T"
      msg="This is clearly not an irc session. Press a key to quit."
      ;;
    "-r"|"--retry")
      pre="retryonfail -p"
      msg="ssh session died. Reconnect?"
      ;;
  esac
  $pre "$msg" kssh -t benley@catbus.corp.google.com screen -RDU irc
}

function waitonfail() {
  local msg="Well, that didn't work. Press a key to quit."
  if [[ "$1" == "-T" ]]; then
    msg="$2"; shift 2
  fi
  $@ || read -s -n1 -p "$msg"; echo
  return 1
}

function retryonfail() {
  if [[ "$1" == "-p" ]]; then
    local prompt=1
    local msg="$2"
    if [[ -z "$3" ]]; then
      echo "retryonfail: message not supplied after -p" > /dev/stderr
      return
    fi
    shift 2
  fi
  while ((1)); do
    $@ && break
    if ((prompt)); then
      read -n1 -p "$msg [Yn]: " response
      [[ $response == "n" ]] && break
      printf "\nRetrying...\n"
    fi
  done
  echo
}

# "source benlib.sh kssh blabla" or "benlib.sh kssh blabla" will work.
$@
