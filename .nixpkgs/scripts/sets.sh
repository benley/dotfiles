#!/bin/bash
# $Id: //depot/ops/corp/scripts/sets.sh#1 $
# Copyright 2008 Google Inc. All Rights Reserved.
#
# Some simple bash set operations.
# These only handle two sets, each with elements separated by $IFS.  Feel free
# to rewrite and generalize.

# Author: Benjamin Staffin <benley@google.com>

# To use:
# source this script.
# call functions like so:
# intersect <shell_variable1> <shell_variable2>
# note that the variables you pass do not have a $ in front of them.

union()
{
  one=$(eval echo '$'$1);
  two=$(eval echo '$'$2);
  echo $one $two | tr " " "\n" | sort | uniq
}

# Duh.  Here only for completeness.
join()
{
  one=$(eval echo '$'$1);
  two=$(eval echo '$'$2);
  echo $one $two
}

subtract()
{
  one=$(eval echo '$'$1);
  two=$(eval echo '$'$2);
  echo $one $two $two | tr " " "\n" | sort | uniq -u
}

intersect()
{
  one=$(eval echo '$'$1);
  two=$(eval echo '$'$2);
  echo $one $two | tr " " "\n" | sort | uniq -d
}
