#!/usr/bin/perl

# This file is to extract all the paragraphs within a file which are seperated by two white lines. 
# examples are: 
# sentence a 
# 
# sentence b
# 
# sentence c
open(FILE, "$ARGV[0]"); 
open(TEMP, ">.tmp");

while(<FILE>) {
    s/^$//g; 			# Remove all the white lines; 
    s/  +/ /g; 			# replace multiple spaces into one. 
    s/\n/ /g;			# Replace newline with space. 
    s/^\s+(.*)$/\1/g;			# Remove the leading spaces. 
    # print $_;
    if ($_ =~ m/PARAGRAPH_[0-9]{1,3}/) {
	print TEMP "\n";
    }
    print TEMP $_;
    # Match the "PARAGRAPH_" pattern. 
    # if ($_ =~ s/(.*)(PARAGRAPH_[0-9]{1,3})(.*)/\1 \2 \2 \3/g) {
    # 	print $_."\n";
    # }
}
close(TEMP); 

# Open the temporary file and process it.
open(TEMP, ".tmp");

while(<TEMP>) {
    if ($_ =~ m/(PARAGRAPH_[0-9]{1,3})(.*)\n/) {
	print $1 . $2 . "\n\n"; 
    }
}

close(TEMP);
close(FILE);
