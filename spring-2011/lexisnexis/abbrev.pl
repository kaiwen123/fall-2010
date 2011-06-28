#!/usr/bin/perl -w
# This script is to abbrevations within the paragrpah; 
# The goal is to differentiate between common abbrevations
# and the sentence boundaries. 

while (<STDIN>) {
    # preprocessing. 
    # if (m/(( [^ \.]+){3})\.([A-Z][^ \.]+ .{20})/) {
    # 	printf "%40s.%-40s\n", $1, $3;
    # 	printf "%40s. %-40s\n", $1, $3;
    # }
    # print $_ . "\n"; 
    while ($_ =~ / ([A-Z][A-Za-z_]*)\. /g) {
	$abbr = lc($1); 
	print $abbr . "\n";
    }
}
