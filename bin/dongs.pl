#!/usr/bin/perl

# takes input on standard in with a number on each line
# and outputs the result in standard ascii chart format

# This is useful for things like:
# xargs wc -l *.txt | grep -v " total$" | dongs.pl

# if, say, 'file' contains:

# 5 bob
# jeph 34
# zach 63 (whoa)
# 11 ed
# 4

# then cat file | dongs.pl
# results in:

# bob         8===D
# jeph        8=====================D
# zach (whoa) 8=======================================D
# ed          8======D
# 4           8==D

# By Randall Munroe, although he denies it.  Hacked together while sitting
# on Jeph Jacques' living room floor while waiting for him to wake up.
# Thank you to his rabbits, who kept me company and helped with debugging.

use List::Util qw(first max maxstr min minstr reduce shuffle sum);

my $width=50; #there's no easy way to get the $COLUMNS variable

my @nums=();
my @labels=();

while(<>)
{
    $num=$_;
    $num=~s/^[^0-9]*([0-9]+)[^0-9]*.*/\1/;
    $num=int($num); 
    $_=~s/^([^0-9]*)[0-9]+([^0-9]*.*)$/\1\2/;
    chomp($_);
    $_=~s/^\s+//g;
    $_=~s/\s+$//g;
    $_=~s/  +/ /g;
    push(@nums,$num);
    push(@labels, $_);
}

my $i;

my $maximum=max(@nums);
my $labelmax=max(@labels);

$maxlen=0;
for $i (@labels)
{
    if($maxlen<length($i))
    {
	$maxlen=length($i)
    }
}

if($maxlen<int(1+log($maximum)/log(10)))
{
    $maxlen=int(log($maximum)/log(10))+1;
}

for($i=0;$i<scalar(@nums);$i++)
{
    $pad=' 'x($maxlen-length($labels[$i]));
    $title=$labels[$i].$pad;
    if ($title =~ /^\s*$/)
    {
#	print $maxlen, "<--";
	$title=$nums[$i].' 'x($maxlen-int(1+log($nums[$i])/log(10)));
    }
    print $title." 8"."=" x (($width-$maxlen)*$nums[$i]/$maximum)."D\n";
}
