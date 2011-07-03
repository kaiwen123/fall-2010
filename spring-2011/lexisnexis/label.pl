#!/usr/bin/perl -w
# This script is to handle the labels: (FN|HN|S|L)_[0-9]+ 
# Examples are shown as follows: 
# 

print @INC;
while (<STDIN>) {
    # preprocessing. 
    if ($_ = /(( [^ \.]+){3})\.([A-Z][^ \.]+ .{20})/) {
	printf "%40s.%-40s\n", $1, $3;
    }
}
