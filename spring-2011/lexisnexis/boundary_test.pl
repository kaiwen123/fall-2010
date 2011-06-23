#!/usr/bin/perl -w

# This script is to test the boundary of sentences. 
# Following functions are done here. 
# 
# - extract possible boundary marks such as [\."\'\?] etc. 
# - analyze the boundaries and find the one that is most 
#   frequently appear in the document. 

while (<STDIN>) {
    # preprocessing. 
    s/  +/ /g;
    $boundary = "[\.\?]";

    # match the boundary of sentence, print words around sentence. 
    # if ($_ =~ m/(( [^ ]+){3})($boundary) (([A-Z][^ ]+ ){3})/) {
    # 	printf "%40s %s %-40s\n", $1, $3, $4; 
    # }

    # match boundary, print fixed length of chars around boundary.
    my $count = 10; 
    if ($_ =~ m/(.{20})($boundary) ([A-Z].{19})/) {
	printf "%20s%s %-20s\n", $1, $2, $3; 
    }
}
