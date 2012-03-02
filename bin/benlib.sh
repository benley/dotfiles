#!/bin/bash

function GetCreds() {
  echo "Had to kinit."
  kinit benley@UNIX.CORP.GOOGLE.COM
}

function kderp() {
  kinit -R &>/dev/null || GetCreds
}
