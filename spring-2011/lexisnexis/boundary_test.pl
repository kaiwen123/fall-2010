#!/usr/bin/perl -w

# This script is to test the boundary of sentences. 
# Following functions are done here. 
# 
# - extract possible boundary marks such as [\."\'\?] etc. 
# - analyze the boundaries and find the one that is most 
#   frequently appear in the document. 

while (<STDIN>) {
    s/  +/ /g;
    if ($_ =~ m/ ([^ ]+)([\.\"\?])( ?[^ ]+) /) {
	print $1 . $2 . $3 . "\n"; 
    }
}
