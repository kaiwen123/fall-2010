#!/usr/bin/perl -w 
# 
# get acronyms from the input file. 
# 
# ==============================

while (<STDIN>) {
    s/  +/ /g;
    if ($_ =~ m/ ([A-Z][a-z0-9]*\.) /) {
	print $1 . "\n"; 
    }
}
