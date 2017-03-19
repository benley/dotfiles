#!bash
source ~/.bashrc

((RANDOM % 100 < 10)) && [[ $(type -t doge != "") ]] && doge

: # always end with $?=0
