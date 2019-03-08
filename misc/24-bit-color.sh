#!/usr/bin/env bash
# This file was originally taken from iterm2 https://github.com/gnachman/iTerm2/blob/master/tests/24-bit-color.sh
#
#   This script echoes a bunch of 24-bit color codes to the terminal
#   to demonstrate its functionality.
#
#   The foreground escape sequence is ^[38;2;<r>;<g>;<b>m
#
#   The background escape sequence is ^[48;2;<r>;<g>;<b>m
#
#   <r> <g> <b> range from 0 to 255 inclusive.
#
#   The escape sequence ^[0m returns output to default

char=" "

printColor()  { printf '\x1b[48;2;%sm%s' "$1" "$char"; }
resetOutput() { printf "\x1b[0m\n"; }

# Gives a color $1/255 % along HSV
# Who knows what happens when $1 is outside 0-255
# Echoes "$red $green $blue" where
# $red $green and $blue are integers
# ranging between 0 and 255 inclusive
rainbowColor() {
  ((
    h = $1 / 43,
    f = $1 - 43 * h,
    t = f * 255 / 43,
    q = 255 - t
  ))

  case $h in
    0) printf '255;%d;0' $t ;;
    1) printf '%d;255;0' $q ;;
    2) printf '0;255;%d' $t ;;
    3) printf '0;%d;255' $q ;;
    4) printf '%d;0;255' $t ;;
    5) printf '255;0;%d' $q ;;
    *)
      # execution should never reach here
      echo "Something weird happened" >&2
      printf '0;0;0'
      ;;
  esac
}

for i in {0..127};   do printColor "$i;0;0";             done; resetOutput
for i in {255..128}; do printColor "$i;0;0";             done; resetOutput
for i in {0..127};   do printColor "0;$i;0";             done; resetOutput
for i in {255..128}; do printColor "0;$i;0";             done; resetOutput
for i in {0..127};   do printColor "0;0;$i";             done; resetOutput
for i in {255..128}; do printColor "0;0;$i";             done; resetOutput
for i in {0..127};   do printColor "$(rainbowColor $i)"; done; resetOutput
for i in {255..128}; do printColor "$(rainbowColor $i)"; done; resetOutput
