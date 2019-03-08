#!/bin/bash -i
# Draws a mandelbrot set.
# Author: Benjamin Staffin
#
# Simulates floating point by using big integers.  Flagrantly uses two integers
# instead of complex numbers.  I couldn't be arsed to calculate infinity limits
# of logarithmic functions in bash, so this just uses escape time values for
# colors.
#
# Algorithm reference: http://en.wikipedia.org/wiki/Mandelbrot_set#For_programmers
# Colors: http://en.wikipedia.org/wiki/ANSI_escape_code#Colors

L=99  # Iteration limit. Even on a 238x61 terminal, 99 is plenty.
P=100000000
Q=$(( P/100 ))
X=$(( Q*320 / ($COLUMNS-1) ))
Y=$(( Q*210 / $LINES ))
y=$(( Q*-105 ))
v=$(( Q*-220 ))
x=$v

# "pixel" 0,0 is the top-left corner of our character grid.

# Outer loop: lines (y values)
while (( y<105*Q )); do

  # Inner loop: columns (x values)
  while (( x<P )); do
    (( a=b=i=c=0 ))
    while (( a*a + b*b < 4*P**2 && i++ < L )); do
      (( c=a,
         a=(a**2 - b**2)/P + x,
         b=2*b*c/P + y ))
    done

    # Color selection via escape values.
    # We stop at $L iterations, so this sets the color of the "lake":
    if (( i >= L )); then j=0; else (( j=i%16 )); fi

    # k controls regular vs bright colors.
    if (( j>7 )); then (( k=1, j-= 8 )); else k=0; fi

    # ANSI SGR color codes start at 30, hence j+30 here:
    printf "\E[$k;$((j+30))m#"

    (( x+=X ))  # okay, next...
  done
  printf '\E[0m'

  (( x=v, y+=Y ))  # On to the next line!
done

# If you think this is absurd, here it is in awk!
# http://rosettacode.org/wiki/Mandelbrot_set#AWK
