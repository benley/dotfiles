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
  pre=""
  msg=""
  if [[ "$1" == "-p" ]]; then
    pre="waitonfail -T"
    msg="This is clearly not an irc session. Press a key to quit."
  fi
  $pre "$msg" kssh -t benley@catbus.corp.google.com screen -RDU irc
}

function waitonfail() {
  msg="Well, that didn't work. Press a key to quit."
  if [[ "$1" == "-T" ]]; then
    msg="$2"; shift 2
  fi
  $@ || read -s -n1 -p "$msg"; echo
  return 1
}

# "source benlib.sh kssh blabla" or "benlib.sh kssh blabla" will work.
$@
