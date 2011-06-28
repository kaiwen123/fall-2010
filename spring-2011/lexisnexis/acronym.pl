#!/usr/bin/perl -w 
# 
# get acronyms from the input file. 
# 
# ==============================

while (<STDIN>) {
    s/  +/ /g;
    while ($_ =~ m/ ([A-Za-z0-9]+\.) /) {
	print $1 . "\n"; 
    }
}
