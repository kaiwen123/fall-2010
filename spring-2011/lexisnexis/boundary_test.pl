#!/usr/bin/perl -w

# This script is to test the boundary of sentences. 
# Following functions are done here. 
# 
# - extract possible boundary marks such as [\."\'\?] etc. 
# - analyze the boundaries and find the one that is most 
#   frequently appear in the document. 

while (<STDIN>) {
    s/  +/ /g;

    # match the boundary of sentence. 
    if ($_ =~ m/(( [^ ]+){3})\. (([A-Z][^ ]+ ){3})/) {
	printf "%10s . %-10s\n", $1, $3; 
    }
    # if ($_ =~ m/ ([^ ]+)([\.\"\?])( ?[^ ]+) /) {
    # 	print $1 . $2 . $3 . "
    # }
}
